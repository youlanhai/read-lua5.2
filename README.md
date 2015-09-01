

luac用法

    usage: ./luac [options] [filenames]
    Available options are:
      -l       list (use -l -l for full listing)
      -o name  output to file 'name' (default is "luac.out")
      -p       parse only
      -s       strip debug information
      -v       show version information
      --       stop handling options
      -        stop handling options and process stdin

打印编译中间步骤
`./luac -l -p exampe.lua`

打印更详细的中间步骤
`./luac -l -l -p exampe.lua`