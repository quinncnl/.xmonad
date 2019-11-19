--
-- File     : ~/.xmonad/xmonad.hs
-- Author   : Yiannis Tsiouris (yiannist)
-- Desc     : A clean and well-documented xmonad configuration file (based on
--            the $HOME/.cabal/share/xmonad-0.10.1/man/xmonad.hs template file).
--
--            It uses:
--              * a ScratchPad (for a hidden terminal),
--              * an IM layout for Pidgin,
--              * a layout prompt (with auto-complete).
--
import           XMonad                          hiding ((|||))
import           XMonad.Config.Xfce
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Accordion
import           XMonad.Layout.DecorationMadness
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Input
import qualified XMonad.StackSet                 as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Scratchpad
import           XMonad.Util.Run(spawnPipe)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.Place

import           Data.Ratio                      ((%))

main :: IO ()
main = do
  xmproc <- spawnPipe "/home/clear/.cabal/bin/xmobar /home/clear/.xmonad/xmobarrc.hs"
  xmonad $ ewmh xfceConfig
    { terminal = "urxvt"
    , modMask = mod4Mask
    , borderWidth = 1
    , focusedBorderColor = "#ff0000"
    , layoutHook = myLayoutHook
    , manageHook = manageHook xfceConfig <+> myManageHook
    } `additionalKeys` myKeys
  where
      -- keybindings
      myKeys = [ ((mod4Mask,                 xK_c           ), spawn "google-chrome")
               , ((mod4Mask,                 xK_r           ), spawn "emacs")
               , ((mod4Mask,                 xK_f           ), spawn "thunar")
               , ((mod4Mask,                 xK_bracketleft ), spawn "pidgin")
               , ((mod4Mask,                 xK_u           ), scratchpad)
               , ((mod4Mask,                 xK_y           ), focusUrgent)
               , ((mod4Mask .|. controlMask, xK_space       ), myLayoutPrompt)
               ]
      scratchpad = scratchpadSpawnActionTerminal "urxvt"

      -- layouts
      myLayoutHook = avoidStrutsOn [U] $
                     smartBorders $
                     onWorkspace "8" imLayout $
                     tall ||| wide ||| full ||| circle ||| sTabbed ||| acc
      tall   = renamed [Replace "tall"] $ Tall 1 0.03 0.5
      wide   = renamed [Replace "wide"] $ Mirror tall
      full   = renamed [Replace "full"] $ Full
      circle = renamed [Replace "circle"] $ circleSimpleDefaultResizable
      sTabbed = renamed [Replace "tabbed"] $ simpleTabbed
      acc = renamed [Replace "accordion"] $ Accordion
      imLayout = withIM (1%7) pidginRoster Grid
      pidginRoster = ClassName "Pidgin" `And` Role "buddy_list"

      -- layout prompt (w/ auto-completion and all layouts)
      myLayoutPrompt = inputPromptWithCompl myXPConfig "Layout"
                       (mkComplFunFromList' allLayouts)
                       ?+ (sendMessage . JumpToLayout)
      myXPConfig = defaultXPConfig { autoComplete = Just 1000 }
      allLayouts = ["tall", "wide", "circle", "full", "tabbed", "accordion"]

      -- manageHook
      myPlacement = withGaps (16,0,16,0) (smart (0.5,0.5))

      myManageHook =     manageDocks
                     <+> placeHook myPlacement
                     <+> floatHook
                     <+> fullscreenManageHook
                     <+> scratchpadManageHookDefault
      -- Inspect with xprop: appName is the first element in WM_CLASS, while
      -- className is the second.
      floatHook = composeAll [ appName =? "gimp-2.8"    --> doFloat
                             , className =? "Iceweasel" --> doF (W.shift "1")
                             , appName =? "NaviSimulator"   --> doF (W.shift "4")
                             , appName =? "xfrun4"      --> doFloat
                             , appName =? "NavApp"      --> doFloat
                             ]
