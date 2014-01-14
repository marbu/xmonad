Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , position = Top
       , lowerOnStart = True
       , persistent = False
       , hideOnStart = False
       , commands = [ Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%a %Y-%m-%d %H:%M:%S" "date" 10
                    , Run StdinReader
                    , Run Battery ["-L" ,"40" ,
                                   "-H" , "80" ,
                                   "--high" , "#969896" ,
                                   "--normal" , "#7aa6da" ,
                                   "--low" , "#d54e53" ,
                                   "-t" , "<left>"] 100
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% | %memory% | %battery% | <fc=#ee9a00>%date%</fc>"
       }
