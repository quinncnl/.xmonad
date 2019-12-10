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
import           XMonad.Config.Kde
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
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
import           XMonad.Hooks.FadeInactive

import XMonad.Layout.ResizableTile
import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map        as M

import           Data.Ratio                      ((%))

--Fading
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
   where fadeAmount = 0.9

myLayout = avoidStruts $ onWorkspace "1" (resizableTile ||| Mirror resizableTile) $ smartBorders (resizableTile ||| Mirror resizableTile ||| Full)
     where
        resizableTile = ResizableTall nmaster delta ratio []
        nmaster = 1
        ratio = toRational (2/(1+sqrt(5)::Double))
        delta = 1/100

main :: IO ()
main = do
  xmproc <- spawnPipe "/home/clear/.cabal/bin/xmobar /home/clear/.xmonad/xmobarrc.hs"
  xmonad $ ewmh kde4Config
    { terminal = "urxvt"
    , modMask = mod4Mask
    , borderWidth = 1
    , focusedBorderColor = "#ff0000"
    , logHook = myLogHook
    , layoutHook = myLayout
    , manageHook = manageHook kde4Config <+> myManageHook
    } `additionalKeys` myKeys
  where
      -- keybindings
      myKeys = [ ((mod4Mask,                 xK_c           ), spawn "google-chrome")
               , ((mod4Mask,                 xK_r           ), spawn "emacs")
               , ((mod4Mask,                 xK_f           ), spawn "thunar")
               , ((mod4Mask,                 xK_bracketleft ), spawn "pidgin")
               , ((mod4Mask,                 xK_u           ), scratchpad)
               , ((mod4Mask,                 xK_y           ), focusUrgent)

               ]
      scratchpad = scratchpadSpawnActionTerminal "urxvt"


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
                             , appName =? "google-chrome" --> doF (W.shift "1")
                             , appName =? "emacs" --> doF (W.shift "2")
                             , appName =? "NaviSimulator"   --> doF (W.shift "4")
                             , appName =? "xfrun4"      --> doFloat
                             , appName =? "NavApp"      --> doFloat
                             ]
