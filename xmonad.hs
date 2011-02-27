-- ~/.xmonad/xmonad.hs
-- viz: http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/martin/.xmobarrc"
  xmonad $ defaultConfig
    { borderWidth = 2
    , modMask = mod4Mask
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
    } `additionalKeys`
    [ ((mod1Mask, xK_l), spawn "xscreensaver-command -lock")
    , ((mod1Mask, xK_d), spawn "/home/martin/bin/qstardict-show-hide.sh")
    -- , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    -- , ((0, xK_Print), spawn "scrot")
    ]
