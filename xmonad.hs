import           XMonad                          hiding ((|||))
import           XMonad.Config.Kde
import           XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Spacing
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.DecorationMadness
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.Tabbed
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
   where fadeAmount = 1

main :: IO ()
main = do
  xmproc <- spawnPipe "/home/clear/.cabal/bin/xmobar /home/clear/.xmonad/xmobarrc.hs"
  xmproc <- spawnPipe "nitrogen --restore"
  xmproc <- spawnPipe "google-chrome"
  xmproc <- spawnPipe "emacs"
  xmonad $ ewmh kde4Config
    { terminal = "urxvt"
    , modMask = mod4Mask
    , borderWidth = 1
    , focusedBorderColor = "#ff0000"
    , logHook = myLogHook
    , manageHook = manageHook kde4Config <+> myManageHook
    } `additionalKeys` myKeys
  where
      -- keybindings
      myKeys = [ ((mod4Mask,                 xK_c           ), spawn "google-chrome")
               , ((mod4Mask,                 xK_r           ), spawn "emacs")
               , ((mod4Mask,                 xK_u           ), scratchpad)
               , ((mod4Mask,                 xK_y           ), focusUrgent)
               ]
      scratchpad = scratchpadSpawnActionTerminal "urxvt"

      myManageHook =     manageDocks
                     <+> floatHook
                     <+> fullscreenManageHook
                     <+> scratchpadManageHookDefault
      -- Inspect with xprop: appName is the first element in WM_CLASS, while
      -- className is the second.
      floatHook = composeAll [ appName =? "gimp-2.8"    --> doFloat
                             , appName =? "google-chrome" --> doF (W.shift "1")
                             , appName =? "emacs" --> doF (W.shift "2")
                             , appName =? "urxvt" --> doF (W.shift "3")
                             , appName =? "NaviSimulator"   --> doF (W.shift "4")
                             , appName =? "NavApp"      --> doFloat
                             ]
