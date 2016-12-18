import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog

import XMonad.Actions.FindEmptyWorkspace

import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM

import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.Shell

import System.IO


modkey = mod4Mask
altMask = mod1Mask
-- TODO: clean this up
myManageHook = composeAll
               [ className =? "Do" --> doFloat
                 -- trying out fullscreen flash
               , isFullscreen --> doFullFloat
               , manageDocks
               ]

-- Extend the desktops past just 1-9
moreWorkspaces = map show [10 .. 12]
moreKeys = [xK_0, xK_bracketleft, xK_bracketright]
myWorkspaces = map show [1 .. 9] ++ moreWorkspaces

myKeys = [ -- M-m shows the next empty workspace
           ((modkey, xK_m), viewEmptyWorkspace)
         ]
         ++
         -- M-{:digit:} accesses each desktop, +shift moves windows
         [((m .|. modkey, k), windows $ f i)
         | (k, i) <- zip moreKeys moreWorkspaces
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
         ]
         ++
         [
           -- Ctrl-Tab to bring up the prompt
           ((controlMask, xK_Tab), shellPrompt defaultXPConfig)
           -- Ctrl-Alt-l locks the screen
         , ((controlMask .|. altMask, xK_l),
            spawn "gnome-screensaver-command --lock")
           -- Override the default XMonad restart command
         , ((modkey, xK_q),
            spawn "killall xautolock nm-applet trayer; xmonad --recompile && xmonad --restart")
         ]

-- Set up startupHook
myStartupHook :: X()
myStartupHook = do
  -- Lock the screen after being idle.
  spawn "xautolock -time 5 -locker 'gnome-screensaver-command --lock'"
  -- Start up the network manager
  spawn "nm-applet"
  -- Start up the trayer
  spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --transparent true --alpha 0 --tint 0x000000 --height 20"

main = do
  xmproc <- spawnPipe "xmobar"

  xmonad $ defaultConfig
    { terminal = "gnome-terminal --hide-menubar"
    , modMask = modkey
    , manageHook = myManageHook <+> manageHook defaultConfig
    , workspaces = myWorkspaces
    , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor "grey" "" . shorten 50
      }
    , startupHook = myStartupHook
    } `additionalKeys` myKeys
