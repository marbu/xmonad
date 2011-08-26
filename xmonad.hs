-- xmonad.hs of marbu
--
-- inpiration:
-- http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_KDE
-- http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration
--

import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
 
main = xmonad $ kde4Config
 { modMask = mod4Mask -- use the Windows button as mod
 , manageHook = manageHook kdeConfig <+> myManageHook
 , layoutHook = smartBorders $ avoidStruts $ layoutHook kde4Config
 , borderWidth = 2
 } -- `additionalKeys` [ ((mod1Mask, xK_d), spawn "/home/martin/local/bin/qstardict-show-hide.sh") ]
 where
   myManageHook = composeAll . concat $
     [ [ className   =? c --> doFloat           | c <- myFloats]
     , [ title       =? t --> doFloat           | t <- myOtherFloats]
     -- , [ className   =? c --> doF (W.shift "2") | c <- webApps]
     -- , [ className   =? c --> doF (W.shift "3") | c <- ircApps]
     ]
   myFloats      = ["MPlayer", "Gimp"]
   myOtherFloats = ["alsamixer"]
   webApps       = ["Firefox-bin", "Opera"] -- open on desktop 2
   ircApps       = ["Ksirc"]                -- open on desktop 3
