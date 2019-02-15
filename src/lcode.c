/*
** $Id: lcode.c,v 2.62.1.1 2013/04/12 18:48:47 roberto Exp $
** Code generator for Lua
** See Copyright Notice in lua.h
*/


#include <stdlib.h>

#define lcode_c
#define LUA_CORE

#include "lua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstring.h"
#include "ltable.h"
#include "lvm.h"


static int hasjumps(expdesc* e)
{
    return e->true_list != e->false_list;
}

static int isnumeral(expdesc *e)
{
    return (e->kind == VKNUM && e->true_list == NO_JUMP && e->false_list == NO_JUMP);
}

/** 生成OP_LOADNIL指令。将from寄存器开始，将n个寄存器都赋值成nil。*/
void luaK_nil (FuncState *fs, int from, int n)
{
    Instruction *previous;
    int l = from + n - 1;  /* last register to set nil */
    if (fs->pc > fs->lasttarget)    /* no jumps to current position? */
    {
        previous = &fs->f->code[fs->pc - 1];
        if (GET_OPCODE(*previous) == OP_LOADNIL)
        {
            int pfrom = GETARG_A(*previous);
            int pl = pfrom + GETARG_B(*previous);
            if ((pfrom <= from && from <= pl + 1) ||
                    (from <= pfrom && pfrom <= l + 1))    /* can connect both? */
            {
                if (pfrom < from) from = pfrom;  /* from = min(from, pfrom) */
                if (pl > l) l = pl;  /* l = max(l, pl) */
                SETARG_A(*previous, from);
                SETARG_B(*previous, l - from);
                return;
            }
        }  /* else go through */
    }
    luaK_codeABC(fs, OP_LOADNIL, from, n - 1, 0);  /* else no optimization */
}

/** 生成无条件跳转指令。并连接到跳转链上。*/
int luaK_jump (FuncState *fs)
{
    // 将跳转链保存下来，防止指令生成过程被清空。
    int jpc = fs->jpc;  /* save list of jumps to here */
    int j;
    fs->jpc = NO_JUMP; // 设置为无跳转，防止内部生成指令的时候，将跳转链清空了。
    
    j = luaK_codeAsBx(fs, OP_JMP, 0, NO_JUMP); // 生成指令，并返回指令的地址
    
    // 重新连接跳转链。
    luaK_concat(fs, &j, jpc);  /* keep them on hold */
    return j;
}


void luaK_ret (FuncState *fs, int first, int nret)
{
    luaK_codeABC(fs, OP_RETURN, first, nret + 1, 0);
}

/** 生成条件跳转指令。一条件跳转指令，后面会紧跟一条OP_JMP指令。也就是分成功跳转，和失败跳转两种情况。
 *
 */
static int condjump (FuncState *fs, OpCode op, int A, int B, int C)
{
    luaK_codeABC(fs, op, A, B, C); // 生成TEST指令
    return luaK_jump(fs); // 生成后续的JMP指令
}

/** 修正跳转指令的跳转目标。
 *  @param pc 要修正的指令地址
 *  @param dest 要跳转到的地址
 */
static void fixjump (FuncState *fs, int pc, int dest)
{
    Instruction *jmp = &fs->f->code[pc];
    int offset = dest - (pc + 1); // 绝对地址转换为相对地址
    lua_assert(dest != NO_JUMP);
    if (abs(offset) > MAXARG_sBx)
        luaX_syntaxerror(fs->ls, "control structure too long");
    SETARG_sBx(*jmp, offset);
}


/** 返回当前指令位置(pc)，并且将其标记为跳转目标。
** returns current `pc' and marks it as a jump target (to avoid wrong
** optimizations with consecutive instructions not in the same basic block).
*/
int luaK_getlabel (FuncState *fs)
{
    fs->lasttarget = fs->pc;
    return fs->pc;
}

/** 获得pc指令要跳转到的目标位置。通常目标位置其实是下一条跳转指令，这样就构成了一条跳转链表。*/
static int getjump (FuncState *fs, int pc)
{
    int offset = GETARG_sBx(fs->f->code[pc]);
    if (offset == NO_JUMP)  /* point to itself represents end of list */
        return NO_JUMP;  /* end of list */
    else
        // 将跳转偏移转换成绝对位置
        return (pc + 1) + offset; /* turn offset into absolute position */
}

/** 获得跳转控制指令。因为jmp的上一条指令，一般都是TEST，返回上一条Test指令。*/
static Instruction *getjumpcontrol (FuncState *fs, int pc)
{
    Instruction *pi = &fs->f->code[pc];
    // 上一条指令也是跳转指令，则返回上一条指令
    if (pc >= 1 && testTMode(GET_OPCODE(*(pi - 1))))
        return pi - 1;
    else
        return pi;
}


/** 检查跳转链中是否有指令不产生值。
** check whether list has any jump that do not produce a value
** (or produce an inverted value)
*/
static int need_value (FuncState *fs, int list)
{
    for (; list != NO_JUMP; list = getjump(fs, list))
    {
        Instruction i = *getjumpcontrol(fs, list);
        if (GET_OPCODE(i) != OP_TESTSET)
            return 1;
    }
    return 0;  /* not found */
}

/** 给Test指令打补丁，设置目标寄存器（即存放比较结果的寄存器）。
 *      注意区分：OP_TEST和OP_TESTSET指令。前者仅对表达式求值，后者在满足条件后，要将求得的结果写入目标寄存器。
 *  前者用于if等条件判断语句中使用，后者用于赋值语句使用。
 *  @param node 跳转指令所在位置
 *  @param reg  目标寄存器
 *  @return 如果可以在尾部连接其他指令，就返回1。否则返回0.
 */
static int patchtestreg (FuncState *fs, int node, int reg)
{
    Instruction *i = getjumpcontrol(fs, node);
    if (GET_OPCODE(*i) != OP_TESTSET)
        return 0;  /* cannot patch other instructions */
    
    // 如果，要存放的值就在目标寄存器里了，没必要浪费一次赋值操作。用TEST指令就好了。
    if (reg != NO_REG && reg != GETARG_B(*i))
    {
        SETARG_A(*i, reg); // 设置目标寄存器
    }
    else
    {
        // 如果不需要存结果 或者 要存放的值就在目标寄存器里，将OP_TESTSET指令修改为OP_TEST。
        // OP_TEST 指令只用到了A,C寄存器。A为条件变量所在的寄存器，C为要比较的值（0|1）
        /* no register to put value or register already has the value */
        *i = CREATE_ABC(OP_TEST, GETARG_B(*i), 0, GETARG_C(*i));
    }

    return 1;
}


static void removevalues (FuncState *fs, int list)
{
    for (; list != NO_JUMP; list = getjump(fs, list))
        patchtestreg(fs, list, NO_REG);
}

/** 修正跳转链表的辅助方法。
 *  @param list     链表头结点
 *  @param vtarget  跳转目标
 *  @param ret      目标寄存器（存放比较结果）
 *  @param dtarget  默认的跳转目标
 */
static void patchlistaux (FuncState *fs, int list, int vtarget, int reg, int dtarget)
{
    while (list != NO_JUMP)
    {
        int next = getjump(fs, list);
        if (patchtestreg(fs, list, reg))
            fixjump(fs, list, vtarget); // 修改跳转目标
        else
            fixjump(fs, list, dtarget);  /* jump to default target */
        
        // 处理下一条跳转指令
        list = next;
    }
}

/** 修正跳转链的地址，然后清空跳转链。*/
static void dischargejpc (FuncState *fs)
{
    patchlistaux(fs, fs->jpc, fs->pc, NO_REG, fs->pc);
    fs->jpc = NO_JUMP;
}

/** 修正跳转链。 如果跳转目标就是当前的指令位置，将list连接到当前的跳转链表中，暂不修复目标。
 *  否则，修正整个链表的跳转目标。
 */
void luaK_patchlist (FuncState *fs, int list, int target)
{
    if (target == fs->pc)
        luaK_patchtohere(fs, list);
    else
    {
        lua_assert(target < fs->pc);
        patchlistaux(fs, list, target, NO_REG, target);
    }
}

/** 修正要关闭的upvalue。
 *  @param list     跳转链表
 *  @param level    关闭大于这一级的所有upvalue
 */
LUAI_FUNC void luaK_patchclose (FuncState *fs, int list, int level)
{
    // 0 是用来表示不关闭upvalue。所以+1来区分。
    level++;  /* argument is +1 to reserve 0 as non-op */
    while (list != NO_JUMP)
    {
        int next = getjump(fs, list);
        lua_assert(GET_OPCODE(fs->f->code[list]) == OP_JMP &&
                   (GETARG_A(fs->f->code[list]) == 0 ||
                    GETARG_A(fs->f->code[list]) >= level));
        SETARG_A(fs->f->code[list], level);
        list = next;
    }
}

/** 将`list`连接到jpc链表尾部。*/
void luaK_patchtohere (FuncState *fs, int list)
{
    luaK_getlabel(fs);
    luaK_concat(fs, &fs->jpc, list);
}


/** 连接待定的跳转指令。将l2连接到l1上，即jpc->l1->l2*/
void luaK_concat (FuncState *fs, int *l1, int l2)
{
    if (l2 == NO_JUMP) return;
    else if (*l1 == NO_JUMP)
        *l1 = l2; //将l2连接到l1上
    else
    {
        // 找到链表的尾结点，将l2接到尾结点上。
        int list = *l1;
        int next;
        while ((next = getjump(fs, list)) != NO_JUMP)  /* find last element */
            list = next;
        fixjump(fs, list, l2);
    }
}

/** 存贮指令i，并且修正跳转链表。当加入非跳转指令的时候，要把之前的跳转链都修复了。
 *  当加入条件跳转指令的时候，会自动保存跳转链，等新指令生成完毕后，再合并到一起(见luaK_jump)。
 */
static int luaK_code (FuncState *fs, Instruction i)
{
    Proto *f = fs->f;
    dischargejpc(fs);  /* `pc' will change */
    
    // 存贮指令
    /* put new instruction in code array */
    luaM_growvector(fs->ls->L, f->code, fs->pc, f->sizecode, Instruction,
                    MAX_INT, "opcodes");
    f->code[fs->pc] = i;
    
    // 存贮行号信息
    /* save corresponding line information */
    luaM_growvector(fs->ls->L, f->lineinfo, fs->pc, f->sizelineinfo, int,
                    MAX_INT, "opcodes");
    f->lineinfo[fs->pc] = fs->ls->lastline;
    return fs->pc++;
}


int luaK_codeABC (FuncState *fs, OpCode o, int a, int b, int c)
{
    lua_assert(getOpMode(o) == iABC);
    lua_assert(getBMode(o) != OpArgN || b == 0);
    lua_assert(getCMode(o) != OpArgN || c == 0);
    lua_assert(a <= MAXARG_A && b <= MAXARG_B && c <= MAXARG_C);
    return luaK_code(fs, CREATE_ABC(o, a, b, c));
}


int luaK_codeABx (FuncState *fs, OpCode o, int a, unsigned int bc)
{
    lua_assert(getOpMode(o) == iABx || getOpMode(o) == iAsBx);
    lua_assert(getCMode(o) == OpArgN);
    lua_assert(a <= MAXARG_A && bc <= MAXARG_Bx);
    return luaK_code(fs, CREATE_ABx(o, a, bc));
}


static int codeextraarg (FuncState *fs, int a)
{
    lua_assert(a <= MAXARG_Ax);
    return luaK_code(fs, CREATE_Ax(OP_EXTRAARG, a));
}

/** 将常量加载到目标寄存器。
 *  @param reg  目标寄存器
 *  @param k    常量寄存器
 */
int luaK_codek (FuncState *fs, int reg, int k)
{
    if (k <= MAXARG_Bx)
        return luaK_codeABx(fs, OP_LOADK, reg, k);
    else
    {
        int p = luaK_codeABx(fs, OP_LOADKX, reg, 0);
        codeextraarg(fs, k);
        return p;
    }
}


void luaK_checkstack (FuncState *fs, int n)
{
    int newstack = fs->freereg + n;
    if (newstack > fs->f->maxstacksize)
    {
        if (newstack >= MAXSTACK)
            luaX_syntaxerror(fs->ls, "function or expression too complex");
        fs->f->maxstacksize = cast_byte(newstack);
    }
}

/** 分配n个寄存器。*/
void luaK_reserveregs (FuncState *fs, int n)
{
    luaK_checkstack(fs, n);
    fs->freereg += n;
}

/** 释放一个寄存器。reg通常是临时变量寄存器。 */
static void freereg (FuncState *fs, int reg)
{
    if (!ISK(reg) && reg >= fs->nactvar)
    {
        fs->freereg--;
        lua_assert(reg == fs->freereg);
    }
}

/** 释放exp占用的寄存器（如果有占用，就释放）。*/
static void freeexp (FuncState *fs, expdesc *e)
{
    if (e->kind == VNONRELOC)
        freereg(fs, e->u.info);
}


static int addk (FuncState *fs, TValue *key, TValue *v)
{
    lua_State *L = fs->ls->L;
    TValue *idx = luaH_set(L, fs->h, key);
    Proto *f = fs->f;
    int k, oldsize;
    if (ttisnumber(idx))
    {
        lua_Number n = nvalue(idx);
        lua_number2int(k, n);
        if (luaV_rawequalobj(&f->k[k], v))
            return k;
        /* else may be a collision (e.g., between 0.0 and "\0\0\0\0\0\0\0\0");
           go through and create a new entry for this value */
    }
    /* constant not found; create a new entry */
    oldsize = f->sizek;
    k = fs->nk;
    /* numerical value does not need GC barrier;
       table has no metatable, so it does not need to invalidate cache */
    setnvalue(idx, cast_num(k));
    luaM_growvector(L, f->k, k, f->sizek, TValue, MAXARG_Ax, "constants");
    while (oldsize < f->sizek) setnilvalue(&f->k[oldsize++]);
    setobj(L, &f->k[k], v);
    fs->nk++;
    luaC_barrier(L, f, v);
    return k;
}

//在常量表中查找字符串。如果字符串不存在，将其添加到常量表。
int luaK_stringK (FuncState *fs, TString *s)
{
    TValue o;
    setsvalue(fs->ls->L, &o, s);
    return addk(fs, &o, &o);
}


int luaK_numberK (FuncState *fs, lua_Number r)
{
    int n;
    lua_State *L = fs->ls->L;
    TValue o;
    setnvalue(&o, r);
    if (r == 0 || luai_numisnan(NULL, r))    /* handle -0 and NaN */
    {
        /* use raw representation as key to avoid numeric problems */
        setsvalue(L, L->top++, luaS_newlstr(L, (char *)&r, sizeof(r)));
        n = addk(fs, L->top - 1, &o);
        L->top--;
    }
    else
        n = addk(fs, &o, &o);  /* regular case */
    return n;
}


static int boolK (FuncState *fs, int b)
{
    TValue o;
    setbvalue(&o, b);
    return addk(fs, &o, &o);
}


static int nilK (FuncState *fs)
{
    TValue k, v;
    setnilvalue(&v);
    /* cannot use nil as key; instead use table itself to represent nil */
    sethvalue(fs->ls->L, &k, fs->h);
    return addk(fs, &k, &v);
}


void luaK_setreturns (FuncState *fs, expdesc *e, int nresults)
{
    if (e->kind == VCALL)    /* expression is an open function call? */
    {
        SETARG_C(getcode(fs, e), nresults + 1);
    }
    else if (e->kind == VVARARG)
    {
        SETARG_B(getcode(fs, e), nresults + 1);
        SETARG_A(getcode(fs, e), fs->freereg);
        luaK_reserveregs(fs, 1);
    }
}


void luaK_setoneret (FuncState *fs, expdesc *e)
{
    if (e->kind == VCALL)    /* expression is an open function call? */
    {
        e->kind = VNONRELOC;
        e->u.info = GETARG_A(getcode(fs, e));
    }
    else if (e->kind == VVARARG)
    {
        SETARG_B(getcode(fs, e), 2);
        e->kind = VRELOCABLE;  /* can relocate its simple result */
    }
}

/** 为待定的查找操作生成指令。即，生成变量查找指令。
 *  此函数不分配目标寄存器。
 */
void luaK_dischargevars (FuncState *fs, expdesc *e)
{
    switch (e->kind)
    {
    case VLOCAL:
    {
        // 局部变量可以直接访问，不需要生成指令
        e->kind = VNONRELOC;
        break;
    }
    case VUPVAL:
    {
        // 将upvalue值(e->u.info)放到A寄存器中，A寄存器稍后填上。
        e->u.info = luaK_codeABC(fs, OP_GETUPVAL, 0, e->u.info, 0);
        e->kind = VRELOCABLE;
        break;
    }
    case VINDEXED:
    {
        OpCode op = OP_GETTABUP;  /* assume 't' is in an upvalue */
        freereg(fs, e->u.ind.idx);
        if (e->u.ind.vt == VLOCAL)    /* 't' is in a register? */
        {
            freereg(fs, e->u.ind.t);
            op = OP_GETTABLE;
        }
        e->u.info = luaK_codeABC(fs, op, 0, e->u.ind.t, e->u.ind.idx);
        e->kind = VRELOCABLE;
        break;
    }
    case VVARARG:
    case VCALL:
    {
        luaK_setoneret(fs, e);
        break;
    }
    default:
        break;  /* there is one value available (somewhere) */
    }
}


static int code_label (FuncState *fs, int A, int b, int jump)
{
    luaK_getlabel(fs);  /* those instructions may be jump targets */
    return luaK_codeABC(fs, OP_LOADBOOL, A, b, jump);
}

/** 处理待定的加载(查找)指令。生成加载指令，并设置目标寄存器。
 *  @param e    待定的指令信息
 *  @param reg  目标寄存器
 */
static void discharge2reg (FuncState *fs, expdesc *e, int reg)
{
    luaK_dischargevars(fs, e);
    switch (e->kind)
    {
    case VNIL:
    {// load nil
        luaK_nil(fs, reg, 1);
        break;
    }
    case VFALSE:
    case VTRUE:
    {// load true/false
        luaK_codeABC(fs, OP_LOADBOOL, reg, e->kind == VTRUE, 0);
        break;
    }
    case VK:
    {// 加载常量
        luaK_codek(fs, reg, e->u.info);
        break;
    }
    case VKNUM:
    {// 加载数字常量
        luaK_codek(fs, reg, luaK_numberK(fs, e->u.nval));
        break;
    }
    case VRELOCABLE:
    {// 给之前生成的指令设置目标寄存器。
        Instruction *pc = &getcode(fs, e);
        SETARG_A(*pc, reg);
        break;
    }
    case VNONRELOC:
    {// 生成赋值指令OP_MOVE
        if (reg != e->u.info) // 避免自己赋值给自己
            luaK_codeABC(fs, OP_MOVE, reg, e->u.info, 0);
        break;
    }
    default:
    {
        lua_assert(e->k == VVOID || e->k == VJMP);
        return;  /* nothing to do... */
    }
    }
    // 将数据加载到local寄存器之后，后续的操作就针对local寄存器了。
    e->u.info = reg;
    e->kind = VNONRELOC;
}

/** 对于需要分配寄存器的指令。统一生成加载指令，并分配寄存器。*/
static void discharge2anyreg (FuncState *fs, expdesc *e)
{
    if (e->kind != VNONRELOC)
    {
        luaK_reserveregs(fs, 1);
        discharge2reg(fs, e, fs->freereg - 1);
    }
}

/** 给表达式分配寄存器，并且修正跳转地址。*/
static void exp2reg (FuncState *fs, expdesc *e, int reg)
{
    // 分配寄存器
    discharge2reg(fs, e, reg);
    // 如果是比较运算，则连接到true跳转链上
    if (e->kind == VJMP)
        luaK_concat(fs, &e->true_list, e->u.info);  /* put this jump in `t' list */
    // 处理跳转链
    if (hasjumps(e))
    {
        int final;  /* position after whole expression */
        int p_f = NO_JUMP;  /* position of an eventual LOAD false */
        int p_t = NO_JUMP;  /* position of an eventual LOAD true */
        if (need_value(fs, e->true_list) || need_value(fs, e->false_list))
        {
            int fj = (e->kind == VJMP) ? NO_JUMP : luaK_jump(fs);
            p_f = code_label(fs, reg, 0, 1);
            p_t = code_label(fs, reg, 1, 0);
            luaK_patchtohere(fs, fj);
        }
        
        // 将跳转链表都修正到当前指令位置
        final = luaK_getlabel(fs);
        patchlistaux(fs, e->false_list, final, reg, p_f);
        patchlistaux(fs, e->true_list, final, reg, p_t);
    }
    e->false_list = e->true_list = NO_JUMP;
    e->u.info = reg;
    e->kind = VNONRELOC;
}


void luaK_exp2nextreg (FuncState *fs, expdesc *e)
{
    luaK_dischargevars(fs, e);
    freeexp(fs, e);
    luaK_reserveregs(fs, 1);
    exp2reg(fs, e, fs->freereg - 1);
}


int luaK_exp2anyreg (FuncState *fs, expdesc *e)
{
    luaK_dischargevars(fs, e);
    if (e->kind == VNONRELOC)
    {
        if (!hasjumps(e)) return e->u.info;  /* exp is already in a register */
        if (e->u.info >= fs->nactvar)    /* reg. is not a local? */
        {
            exp2reg(fs, e, e->u.info);  /* put value on it */
            return e->u.info;
        }
    }
    luaK_exp2nextreg(fs, e);  /* default */
    return e->u.info;
}


void luaK_exp2anyregup (FuncState *fs, expdesc *e)
{
    if (e->kind != VUPVAL || hasjumps(e))
        luaK_exp2anyreg(fs, e);
}


void luaK_exp2val (FuncState *fs, expdesc *e)
{
    if (hasjumps(e))
        luaK_exp2anyreg(fs, e);
    else
        luaK_dischargevars(fs, e);
}


int luaK_exp2RK (FuncState *fs, expdesc *e)
{
    luaK_exp2val(fs, e);
    switch (e->kind)
    {
    case VTRUE:
    case VFALSE:
    case VNIL:
    {
        if (fs->nk <= MAXINDEXRK)    /* constant fits in RK operand? */
        {
            e->u.info = (e->kind == VNIL) ? nilK(fs) : boolK(fs, (e->kind == VTRUE));
            e->kind = VK;
            return RKASK(e->u.info);
        }
        else break;
    }
    case VKNUM:
    {
        e->u.info = luaK_numberK(fs, e->u.nval);
        e->kind = VK;
        /* go through */
    }
    case VK:
    {
        if (e->u.info <= MAXINDEXRK)  /* constant fits in argC? */
            return RKASK(e->u.info);
        else break;
    }
    default:
        break;
    }
    /* not a constant in the right range: put it in a register */
    return luaK_exp2anyreg(fs, e);
}


void luaK_storevar (FuncState *fs, expdesc *var, expdesc *ex)
{
    switch (var->kind)
    {
    case VLOCAL:
    {
        freeexp(fs, ex);
        exp2reg(fs, ex, var->u.info);
        return;
    }
    case VUPVAL:
    {
        int e = luaK_exp2anyreg(fs, ex);
        luaK_codeABC(fs, OP_SETUPVAL, e, var->u.info, 0);
        break;
    }
    case VINDEXED:
    {
        OpCode op = (var->u.ind.vt == VLOCAL) ? OP_SETTABLE : OP_SETTABUP;
        int e = luaK_exp2RK(fs, ex);
        luaK_codeABC(fs, op, var->u.ind.t, var->u.ind.idx, e);
        break;
    }
    default:
    {
        lua_assert(0);  /* invalid var kind to store */
        break;
    }
    }
    freeexp(fs, ex);
}


void luaK_self (FuncState *fs, expdesc *e, expdesc *key)
{
    int ereg;
    luaK_exp2anyreg(fs, e);
    ereg = e->u.info;  /* register where 'e' was placed */
    freeexp(fs, e);
    e->u.info = fs->freereg;  /* base register for op_self */
    e->kind = VNONRELOC;
    luaK_reserveregs(fs, 2);  /* function and 'self' produced by op_self */
    luaK_codeABC(fs, OP_SELF, e->u.info, ereg, luaK_exp2RK(fs, key));
    freeexp(fs, key);
}

// 反转跳转指令。
static void invertjump (FuncState *fs, expdesc *e)
{
    Instruction *pc = getjumpcontrol(fs, e->u.info);
    lua_assert(testTMode(GET_OPCODE(*pc)) && GET_OPCODE(*pc) != OP_TESTSET &&
               GET_OPCODE(*pc) != OP_TEST);
    SETARG_A(*pc, !(GETARG_A(*pc)));
}

/** 生成条件跳转指令。
 *  @param e    主要记录指令用到的寄存器信息。
 *  @param cond 要判断的条件（0|1）.
 */
static int jumponcond (FuncState *fs, expdesc *e, int cond)
{
    if (e->kind == VRELOCABLE)
    {
        // 优化not指令。如果前一条指令是not，这里直接生成相反的跳转。
        Instruction ie = getcode(fs, e);
        if (GET_OPCODE(ie) == OP_NOT)
        {
            fs->pc--;  /* remove previous OP_NOT */
            //生成TEST跳转指令
            return condjump(fs, OP_TEST, GETARG_B(ie), 0, !cond);
        }
        /* else go through */
    }
    
    // 为条件变量生成加载指令（如果需要加载的话）
    discharge2anyreg(fs, e);
    freeexp(fs, e);
    
    // 生成TESTSET跳转指令
    return condjump(fs, OP_TESTSET, NO_REG, e->u.info, cond);
}

/** 生成跳转指令。如果条件成立，就继续向下执行，否则跳转到指定地址。
 *  @param e 最近一次的表达式结果
 */
void luaK_goiftrue (FuncState *fs, expdesc *e)
{
    int pc;  /* pc of last jump */
    luaK_dischargevars(fs, e);
    switch (e->kind)
    {
    case VJMP:
    {
        invertjump(fs, e);
        pc = e->u.info;
        break;
    }
    case VK:
    case VKNUM:
    case VTRUE:
    {
        //这些值始终是true，没必要生成跳转指令了
        pc = NO_JUMP;  /* always true; do nothing */
        break;
    }
    default:
    {// 生成条件跳转指令.
        pc = jumponcond(fs, e, 0);
        break;
    }
    }
    // 将新生成的跳转指令连接到之前的指令后面
    luaK_concat(fs, &e->false_list, pc);  /* insert last jump in `f' list */
    // 再将整个跳转连接到fs->jpc上。
    luaK_patchtohere(fs, e->true_list);
    e->true_list = NO_JUMP;
}

/** 生成跳转指令。与luaK_goiftrue同理。*/
void luaK_goiffalse (FuncState *fs, expdesc *e)
{
    int pc;  /* pc of last jump */
    luaK_dischargevars(fs, e);
    switch (e->kind)
    {
    case VJMP:
    {
        pc = e->u.info;
        break;
    }
    case VNIL:
    case VFALSE:
    {
        pc = NO_JUMP;  /* always false; do nothing */
        break;
    }
    default:
    {
        pc = jumponcond(fs, e, 1);
        break;
    }
    }
    luaK_concat(fs, &e->true_list, pc);  /* insert last jump in `t' list */
    luaK_patchtohere(fs, e->false_list);
    e->false_list = NO_JUMP;
}


static void codenot (FuncState *fs, expdesc *e)
{
    luaK_dischargevars(fs, e);
    switch (e->kind)
    {
    case VNIL:
    case VFALSE:
    {
        e->kind = VTRUE;
        break;
    }
    case VK:
    case VKNUM:
    case VTRUE:
    {
        e->kind = VFALSE;
        break;
    }
    case VJMP:
    {
        invertjump(fs, e);
        break;
    }
    case VRELOCABLE:
    case VNONRELOC:
    {
        discharge2anyreg(fs, e);
        freeexp(fs, e);
        e->u.info = luaK_codeABC(fs, OP_NOT, 0, e->u.info, 0);
        e->kind = VRELOCABLE;
        break;
    }
    default:
    {
        lua_assert(0);  /* cannot happen */
        break;
    }
    }
    /* interchange true and false lists */
    {
        int temp = e->false_list;
        e->false_list = e->true_list;
        e->true_list = temp;
    }
    removevalues(fs, e->false_list);
    removevalues(fs, e->true_list);
}

//生成指令：在t中索引k。
void luaK_indexed (FuncState *fs, expdesc *t, expdesc *k)
{
    lua_assert(!hasjumps(t));
    t->u.ind.t = t->u.info;
    t->u.ind.idx = luaK_exp2RK(fs, k);
    t->u.ind.vt = (t->kind == VUPVAL) ? VUPVAL
                  : check_exp(vkisinreg(t->k), VLOCAL);
    t->kind = VINDEXED;
}

/** 编译期直接计算出常量表达式 */
static int constfolding (OpCode op, expdesc *e1, expdesc *e2)
{
    lua_Number r;
    if (!isnumeral(e1) || !isnumeral(e2)) return 0;
    if ((op == OP_DIV || op == OP_MOD) && e2->u.nval == 0)
        return 0;  /* do not attempt to divide by 0 */
    r = luaO_arith(op - OP_ADD + LUA_OPADD, e1->u.nval, e2->u.nval);
    e1->u.nval = r;
    return 1;
}


static void codearith (FuncState *fs, OpCode op,
                       expdesc *e1, expdesc *e2, int line)
{
    if (constfolding(op, e1, e2))
        return;
    else
    {
        int o2 = (op != OP_UNM && op != OP_LEN) ? luaK_exp2RK(fs, e2) : 0;
        int o1 = luaK_exp2RK(fs, e1);
        if (o1 > o2)
        {
            freeexp(fs, e1);
            freeexp(fs, e2);
        }
        else
        {
            freeexp(fs, e2);
            freeexp(fs, e1);
        }
        e1->u.info = luaK_codeABC(fs, op, 0, o1, o2);
        e1->kind = VRELOCABLE;
        luaK_fixline(fs, line);
    }
}

/** 为比较运算生成代码
 *  @param cond OP_TEST要判断的条件(0|1)。如 < 和 >，是一对相反的指令，>可以看做not <，因此不需要为>专门定义指令了。
 */
static void codecomp (FuncState *fs, OpCode op, int cond, expdesc *e1,
                      expdesc *e2)
{
    int o1 = luaK_exp2RK(fs, e1);
    int o2 = luaK_exp2RK(fs, e2);
    freeexp(fs, e2);
    freeexp(fs, e1);
    if (cond == 0 && op != OP_EQ)
    {
        int temp;  /* exchange args to replace by `<' or `<=' */
        temp = o1;
        o1 = o2;
        o2 = temp;  /* o1 <==> o2 */
        cond = 1;
    }
    e1->u.info = condjump(fs, op, cond, o1, o2);
    e1->kind = VJMP;
}

/** 修正前缀表达式。*/
void luaK_prefix (FuncState *fs, UnOpr op, expdesc *e, int line)
{
    expdesc e2;
    e2.true_list = e2.false_list = NO_JUMP;
    e2.kind = VKNUM;
    e2.u.nval = 0;
    switch (op)
    {
    case OPR_MINUS:
    {
        //数值常量，直接计算出结果
        if (isnumeral(e))  /* minus constant? */
            e->u.nval = luai_numunm(NULL, e->u.nval);  /* fold it */
        else
        {
            luaK_exp2anyreg(fs, e);
            codearith(fs, OP_UNM, e, &e2, line);
        }
        break;
    }
    case OPR_NOT:
        codenot(fs, e);
        break;
    case OPR_LEN:
    {
        luaK_exp2anyreg(fs, e);  /* cannot operate on constants */
        codearith(fs, OP_LEN, e, &e2, line);
        break;
    }
    default:
        lua_assert(0);
    }
}

/** 修正运算符左侧的表达式。
 *      lua逻辑运算是中断模式，如果逻辑运算符前面的表达式满足条件，后面的表达式就无需求值了。
 *  因此，不管运算符的优先级如何，都要为前面的表达式生成代码。
 *      比如：a = b and c + 1，虽然“+”号运算符优先级更高，理应先为c+1生成代码，但是如果b是false，
 *  则c+1都不用计算了。所有，要先为b生成代码。
 */
void luaK_infix (FuncState *fs, BinOpr op, expdesc *v)
{
    switch (op)
    {
    case OPR_AND:
    {
        luaK_goiftrue(fs, v);
        break;
    }
    case OPR_OR:
    {
        luaK_goiffalse(fs, v);
        break;
    }
    case OPR_CONCAT:
    {
        luaK_exp2nextreg(fs, v);  /* operand must be on the `stack' */
        break;
    }
    case OPR_ADD:
    case OPR_SUB:
    case OPR_MUL:
    case OPR_DIV:
    case OPR_MOD:
    case OPR_POW:
    {
        if (!isnumeral(v)) luaK_exp2RK(fs, v);
        break;
    }
    default:
    {
        luaK_exp2RK(fs, v);
        break;
    }
    }
}

/** 修正运算符右侧的表达式 */
void luaK_posfix (FuncState *fs, BinOpr op,
                  expdesc *e1, expdesc *e2, int line)
{
    switch (op)
    {
    case OPR_AND:
    {
        lua_assert(e1->t == NO_JUMP);  /* list must be closed */
        luaK_dischargevars(fs, e2);
        luaK_concat(fs, &e2->false_list, e1->false_list);
        *e1 = *e2;
        break;
    }
    case OPR_OR:
    {
        lua_assert(e1->f == NO_JUMP);  /* list must be closed */
        luaK_dischargevars(fs, e2);
        luaK_concat(fs, &e2->true_list, e1->true_list);
        *e1 = *e2;
        break;
    }
    case OPR_CONCAT:
    {
        luaK_exp2val(fs, e2);
        if (e2->kind == VRELOCABLE && GET_OPCODE(getcode(fs, e2)) == OP_CONCAT)
        {
            lua_assert(e1->u.info == GETARG_B(getcode(fs, e2)) - 1);
            freeexp(fs, e1);
            SETARG_B(getcode(fs, e2), e1->u.info);
            e1->kind = VRELOCABLE;
            e1->u.info = e2->u.info;
        }
        else
        {
            luaK_exp2nextreg(fs, e2);  /* operand must be on the 'stack' */
            codearith(fs, OP_CONCAT, e1, e2, line);
        }
        break;
    }
    case OPR_ADD:
    case OPR_SUB:
    case OPR_MUL:
    case OPR_DIV:
    case OPR_MOD:
    case OPR_POW:
    {
        codearith(fs, cast(OpCode, op - OPR_ADD + OP_ADD), e1, e2, line);
        break;
    }
    case OPR_EQ:
    case OPR_LT:
    case OPR_LE:
    {
        codecomp(fs, cast(OpCode, op - OPR_EQ + OP_EQ), 1, e1, e2);
        break;
    }
    case OPR_NE:
    case OPR_GT:
    case OPR_GE:
    {
        codecomp(fs, cast(OpCode, op - OPR_NE + OP_EQ), 0, e1, e2);
        break;
    }
    default:
        lua_assert(0);
    }
}


void luaK_fixline (FuncState *fs, int line)
{
    fs->f->lineinfo[fs->pc - 1] = line;
}


void luaK_setlist (FuncState *fs, int base, int nelems, int tostore)
{
    int c =  (nelems - 1) / LFIELDS_PER_FLUSH + 1;
    int b = (tostore == LUA_MULTRET) ? 0 : tostore;
    lua_assert(tostore != 0);
    if (c <= MAXARG_C)
        luaK_codeABC(fs, OP_SETLIST, base, b, c);
    else if (c <= MAXARG_Ax)
    {
        luaK_codeABC(fs, OP_SETLIST, base, b, 0);
        codeextraarg(fs, c);
    }
    else
        luaX_syntaxerror(fs->ls, "constructor too long");
    fs->freereg = base + 1;  /* free registers with list values */
}

