Config {
       font = "xft:Inconsolata:pixelsize=16",
       -- used to make the bar appear correctly after Mod-q in older xmonad implementations (0.9.x)
       -- doesn't seem to do anything anymore (0.10, darcs)
--       lowerOnStart = False,
       commands = [
                Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10,
                Run Memory ["-t","Mem: <usedratio>%"] 10,
                Run Swap [] 10,
                Run Date "%a %b %_d %l:%M" "date" 10,
                Run Network "ra0" [] 10
                ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%ra0% | %cpu% | %memory% * %swap%    <fc=#ee9a00>%date%</fc> | %KADS%"
       }
