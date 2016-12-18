import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig

import XMonad.Actions.FindEmptyWorkspace

import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM

import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet as W

import XMonad.Hooks.ICCCMFocus

import XMonad.Prompt
import XMonad.Prompt.Shell


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
            spawn "killall xautolock; xmonad --recompile && xmonad --restart")
         ]

-- Set up startupHook
myStartupHook :: X()
myStartupHook = do
  -- Lock the screen after being idle.
  spawn "xautolock -time 5 -locker 'gnome-screensaver-command --lock'"

main = xmonad $ defaultConfig
        { terminal = "gnome-terminal --hide-menubar"
        , modMask = modkey
        , manageHook = myManageHook <+> manageHook defaultConfig
        , workspaces = myWorkspaces
        , layoutHook = smartBorders $ layoutHook defaultConfig
        , logHook = takeTopFocus
        , startupHook = myStartupHook
        } `additionalKeys` myKeys
