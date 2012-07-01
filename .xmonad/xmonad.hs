import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Config.Gnome

import XMonad.Actions.FindEmptyWorkspace

import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
-- import XMonad.Layout.FixedColumn

import qualified XMonad.StackSet as W


modkey = mod4Mask
myManageHook = composeAll
               [ className =? "Do" --> doFloat
               , className =? "Pidgin" --> doShift "12"
               , manageDocks
               ]

moreWorkspaces = map show [10 .. 12]
moreKeys = [xK_0, xK_bracketleft, xK_bracketright]
myWorkspaces = map show [1 .. 9] ++ moreWorkspaces

myKeys = [ ((modkey, xK_m), viewEmptyWorkspace)
         ]
         ++
         [((m .|. modkey, k), windows $ f i)
         | (k, i) <- zip moreKeys moreWorkspaces
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
         ]
         
myLayouts = onWorkspace "12" imLayout $ layoutHook gnomeConfig
  where
    imLayout = Tall 1 (3/100) (3/4)
    -- imLayout = FixedColumn 1 20 120 10

main = xmonad $ gnomeConfig
        { terminal = "gnome-terminal"
        , modMask = modkey
        , manageHook = myManageHook <+> manageHook gnomeConfig
        , workspaces = myWorkspaces
        , layoutHook = myLayouts
        } `additionalKeys` myKeys
