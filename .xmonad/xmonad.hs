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

import XMonad.Util.Paste

-- Used for volume key definitions.
import Graphics.X11.ExtraTypes.XF86

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
            spawn "killall xautolock nm-applet volti ibus-daemon trayer redshift redshift-gtk; xmonad --recompile && xmonad --restart")
           -- Define volume keys.
         , ((noModMask, xF86XK_AudioLowerVolume), spawn "amixer set Master 5-")
         , ((noModMask, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5+")
           -- Work around for mute working, but unmute leaving subcomponents muted.
           -- https://bugs.launchpad.net/ubuntu/+source/alsa-utils/+bug/1313813
         , ((noModMask, xF86XK_AudioMute), spawn "amixer -D pulse set Master toggle")
           -- Get a sane tab-switching keybinding (mostly for bujano).
         , ((modkey .|. controlMask, xK_Down), sendKey controlMask xK_Page_Down)
         , ((modkey .|. controlMask, xK_Up), sendKey controlMask xK_Page_Up)
           -- Get a sane home/end keybinding.
         , ((modkey, xK_Left), sendKey noModMask xK_Home)
         , ((modkey, xK_Right), sendKey noModMask xK_End)
         ]

-- Set up startupHook
myStartupHook :: X()
myStartupHook = do
  -- Lock the screen after being idle.
  spawn "xautolock -time 5 -locker 'gnome-screensaver-command --lock'"
  -- Start up the network manager
  spawn "nm-applet"
  -- Start volume control
  spawn "volti"
  -- Start keyboard layout control
  spawn "ibus-daemon"
  -- Start up the trayer
  spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --transparent true --alpha 0 --tint 0x000000 --height 20"
  -- Start up redshift-gtk
  spawn "redshift-gtk"

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
