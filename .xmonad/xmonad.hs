import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Config.Gnome

import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.DynamicWorkspaces

import qualified XMonad.StackSet as W


modkey = mod4Mask
myManageHook = composeAll
               [ className =? "Do" --> doFloat
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

main = xmonad $ gnomeConfig
        { terminal = "gnome-terminal"
        , modMask = modkey
        , manageHook = myManageHook <+> manageHook gnomeConfig
        , workspaces = myWorkspaces
        } `additionalKeys` myKeys
