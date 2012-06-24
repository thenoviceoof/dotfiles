import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Config.Gnome

import XMonad.Actions.FindEmptyWorkspace
-- import XMonad.Actions.DynamicWorkspaces

myManageHook = composeAll
               [ className =? "Do" --> doFloat
               , manageDocks
               ]

-- myWorkspaces = map show [1 .. 9]

moreKeys = [ ("M-m", viewEmptyWorkspace)
           -- , ("M-0", (switchToWorkspace 0))
           ]

main = xmonad $ gnomeConfig
        { terminal = "gnome-terminal"
        , modMask = mod4Mask
        , manageHook = myManageHook <+> manageHook defaultConfig
        } `additionalKeysP` moreKeys
