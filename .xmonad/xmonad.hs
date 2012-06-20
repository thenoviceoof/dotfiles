import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Config.Gnome

myManageHook = composeAll
	[ className =? "Do" --> doFloat
	, manageDocks
	]

main = xmonad $ gnomeConfig
	{ terminal = "gnome-terminal"
	, modMask = mod4Mask
	, manageHook = myManageHook <+> manageHook defaultConfig
	}
