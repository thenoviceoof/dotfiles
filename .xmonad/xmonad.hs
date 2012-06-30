import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Config.Gnome

import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.DynamicWorkspaces

import qualified XMonad.StackSet as W

myManageHook = composeAll
               [ className =? "Do" --> doFloat
               , manageDocks
               ]

myWorkspaces = map show [1 .. 12]

moreKeys = [ ("M-m", viewEmptyWorkspace)
           , ("M-0", windows $ W.greedyView "10")
           , ("M-[", windows $ W.greedyView "11")
           , ("M-]", windows $ W.greedyView "12")
           ]

main = xmonad $ gnomeConfig
        { terminal = "gnome-terminal"
        , modMask = mod4Mask
        , manageHook = myManageHook <+> manageHook gnomeConfig
        , workspaces = myWorkspaces
        } `additionalKeysP` moreKeys
