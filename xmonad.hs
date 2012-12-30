--
-- ~/.xmonad/xmonad.hs
--

-- based on
-- default desktop configuration for Fedora
-- http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_KDE
-- http://haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad

import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce

import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.ResizableTile

import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W

--
-- basic configuration
--

myModMask     = mod4Mask -- use the Windows key as mod
myBorderWidth = 2        -- set window border size

--
-- key bindings
--

myKeys        = []
-- ((mod1Mask, xK_d), spawn "/home/martin/local/bin/qstardict-show-hide.sh")

--
-- hooks for newly created windows
--

myManageHook :: ManageHook
myManageHook = composeAll . concat $
  [ [ className   =? c --> doFloat           | c <- myFloats]
  , [ className   =? c --> doF (W.shift "9") | c <- mailApps]
  ]
  where
    myFloats      = ["MPlayer", "Gimp"]
    mailApps      = ["Thunderbird"]

--
-- desktop :: DESKTOP_SESSION -> desktop_configuration
--

desktop "gnome"         = gnomeConfig
desktop "xmonad-gnome"  = gnomeConfig
desktop "kde"           = kde4Config
desktop "kde-plasma"    = kde4Config
desktop "xfce"          = xfceConfig
desktop _               = desktopConfig

defDesktopConfig = maybe desktopConfig desktop

--
-- main function
--

main :: IO ()
main = do
  session <- getEnv "DESKTOP_SESSION"
  xmonad (defDesktopConfig session)
    { modMask     = myModMask
    , borderWidth = myBorderWidth
    , layoutHook  = smartBorders $ avoidStruts $ layoutHook (defDesktopConfig session)
    , manageHook  = manageDocks <+> myManageHook <+> manageHook (defDesktopConfig session)
    } -- `additionalKeys` myKeys
