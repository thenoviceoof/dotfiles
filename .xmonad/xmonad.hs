import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Config.Gnome

import XMonad.Actions.FindEmptyWorkspace

import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM

import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet as W

import XMonad.Hooks.ICCCMFocus


modkey = mod4Mask
-- TODO: clean this up
myManageHook = composeAll
               [ className =? "Do" --> doFloat
               , className =? "Pidgin" --> doShift "12"
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
         
-- By default, split the last desktop unevenly (pidgin layout)
myLayouts = onWorkspace "12" imLayout $ layoutHook gnomeConfig
  where
    imLayout = Tall 1 (3/100) (3/4)
    -- imLayout = FixedColumn 1 20 120 10

main = xmonad $ gnomeConfig
        { terminal = "gnome-terminal --hide-menubar"
        , modMask = modkey
        , manageHook = myManageHook <+> manageHook gnomeConfig
        , workspaces = myWorkspaces
        , layoutHook = smartBorders $ myLayouts
        , logHook = takeTopFocus
        } `additionalKeys` myKeys
