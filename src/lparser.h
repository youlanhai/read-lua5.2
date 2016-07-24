/*
** $Id: lparser.h,v 1.70.1.1 2013/04/12 18:48:47 roberto Exp $
** Lua Parser
** See Copyright Notice in lua.h
*/

#ifndef lparser_h
#define lparser_h

#include "llimits.h"
#include "lobject.h"
#include "lzio.h"


/*
** Expression descriptor
*/
//表达式类型
typedef enum
{
    VVOID,	/* no value */
    VNIL,
    VTRUE,
    VFALSE,
    VK,		//常量表k中的索引 /* info = index of constant in `k' */
    VKNUM,	//数值 /* nval = numerical value */
    VNONRELOC,	//结果寄存器 /* info = result register */
    VLOCAL,	//局部寄存器 /* info = local register */
    VUPVAL,   // upvalue /* info = index of upvalue in 'upvalues' */
    VINDEXED,	/* t = table register/upvalue; idx = index R/K */
    VJMP,       //info存放的跳转地址 /* info = instruction pc */
    VRELOCABLE,	/* info = instruction pc */
    VCALL,	/* info = instruction pc */
    VVARARG	/* info = instruction pc */
} expkind;


#define vkisvar(k)	(VLOCAL <= (k) && (k) <= VINDEXED)
#define vkisinreg(k)	((k) == VNONRELOC || (k) == VLOCAL)

//表达式描述
typedef struct expdesc
{
    expkind k;
    union
    {
        struct    /* for indexed variables (VINDEXED) */
        {
            //table的索引
            short idx;  /* index (R/K) */
            //table所在的寄存器或者upvalue
            lu_byte t;  /* table (register or upvalue) */
            //vt表示t是寄存器还是upvalue
            lu_byte vt;  /* whether 't' is register (VLOCAL) or upvalue (VUPVAL) */
        } ind;
        int info;  /* for generic use */
        lua_Number nval;  /* for VKNUM */
    } u;
    int t;  /* patch list of `exit when true' */
    int f;  /* patch list of `exit when false' */
} expdesc;


/* description of active local variable */
typedef struct Vardesc
{
    // 变量在‘局部变量栈’中的索引
    short idx;  /* variable index in stack */
} Vardesc;


/* description of pending goto statements and label statements */
typedef struct Labeldesc
{
    TString *name;  /* label identifier */
    int pc;  /* position in code */
    int line;  /* line where it appeared */
    lu_byte nactvar;  /* local level where it appears in current block */
} Labeldesc;


/* list of labels or gotos */
typedef struct Labellist
{
    Labeldesc *arr;  /* array */
    int n;  /* number of entries in use */
    int size;  /* array size */
} Labellist;

/** 动态数据。存放当前编译单元（整个文件）的所有激活的局部变量。*/
/* dynamic structures used by the parser */
typedef struct Dyndata
{
    struct    /* list of active local variables */
    {
        Vardesc *arr;
        int n; //使用中的数量
        int size; //arr的总长度
    } actvar;
    Labellist gt;  /* list of pending gotos */
    Labellist label;   /* list of active labels */
} Dyndata;


/* control of blocks */
struct BlockCnt;  /* defined in lparser.c */


/* state needed to generate code for a given function */
typedef struct FuncState
{
    Proto *f;  /* current function header */
    Table *h;  /* table to find (and reuse) elements in `k' */
    struct FuncState *prev; //闭包函数。即，外层函数。  /* enclosing function */
    struct LexState *ls;  /* lexical state */
    struct BlockCnt *bl;  /* chain of current blocks */
    int pc;  /* next position to code (equivalent to `ncode') */
    int lasttarget;   /* 'label' of last 'jump label' */
    int jpc; //待定的跳转链表 /* list of pending jumps to `pc' */
    int nk;  /* number of elements in `k' */
    int np;  /* number of elements in `p' */
    
    /** 第一个局部变量在Lex动态数组中的偏移位置*/
    int firstlocal;   /* index of first local var (in Dyndata array) */
    /** 所有局部变量的数量*/
    short nlocvars;  /* number of elements in 'f->locvars' */
    /** 活动状态的局部变量数量*/
    lu_byte nactvar;  /* number of active local variables */
    
    lu_byte nups;  /* number of upvalues */
    lu_byte freereg;  /* first free register */
} FuncState;


LUAI_FUNC Closure *luaY_parser (lua_State *L, ZIO *z, Mbuffer *buff,
                                Dyndata *dyd, const char *name, int firstchar);


#endif
