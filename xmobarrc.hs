Config {
       font = "xft:Inconsolata:pixelsize=16",
       -- used to make the bar appear correctly after Mod-q in older xmonad implementations (0.9.x)
       -- doesn't seem to do anything anymore (0.10, darcs)
--       lowerOnStart = False,
       commands = [
                Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10,
                Run Memory ["-t","Mem: <usedratio>%"] 10,
	        Run Battery [
                  	"-t", "<acstatus>: <left>% - <timeleft>",
                  	"--",
                  	--"-c", "charge_full",
                  	"-O", "AC",
                  	"-o", "Bat",
                  	"-h", "green",
                  	"-l", "red"
                  	] 10,

                -- network activity monitor (dynamic interface resolution)
                Run DynNetwork [ "--template" , "<dev>:▲<tx>kB/s|▼<rx>kB/s"
                             , "--Low"      , "100000"       -- 100kB units: B/s
                             , "--High"     , "1000000"       -- 1000kB units: B/s
                             , "--low"      , "red"
                             , "--normal"   , "yellow"
                             , "--high"     , "green"
                             ] 10,
                Run Swap [] 10,
                Run Date "%a %b %_d %l:%M" "date" 50,
                Run MultiCoreTemp ["-t", "Temp: <avg>°C, <avgpc>%",
                                   "-L", "60", "-H", "80",
                                   "-l", "green", "-n", "yellow", "-h", "red",
                                    "--", "--mintemp", "20", "--maxtemp", "100"] 50
                ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%battery% | %multicoretemp% | %cpu% | %memory% * %swap% }{ %dynnetwork% | <fc=#ee9a00>%date%</fc>"
       }
