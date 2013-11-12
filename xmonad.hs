--
-- ~/.xmonad/xmonad.hs
--

-- based on
-- default desktop configuration for Fedora
-- http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_KDE
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)

import XMonad

import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce

import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile

import XMonad.Actions.PhysicalScreens

import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W

--
-- basic configuration
--

myModMask     = mod4Mask -- use the Windows key as mod
myBorderWidth = 2        -- set window border size
myTerminal    = "urxvt"  -- preferred terminal emulator

--
-- key bindings
--

myKeys = [
   ((myModMask, xK_a), sendMessage MirrorShrink) -- for  ResizableTall
 , ((myModMask, xK_z), sendMessage MirrorExpand) -- for  ResizableTall
 , ((myModMask, xK_w), viewScreen 0)
 , ((myModMask, xK_e), viewScreen 1)
 --((myModMask, xK_d), spawn "/home/martin/bin/qstardict-show-hide.sh")
 ]

--
-- hooks for newly created windows
-- note: run 'xprop WM_CLASS' to get className
--

myManageHook :: ManageHook
myManageHook = manageDocks <+> coreManageHook

coreManageHook :: ManageHook
coreManageHook = composeAll . concat $
  [ [ className   =? c --> doFloat           | c <- myFloats]
  , [ className   =? c --> doF (W.shift "9") | c <- mailApps]
  ]
  where
    myFloats      = ["MPlayer", "Gimp", "Plasma-desktop", "Klipper"]
    mailApps      = ["Thunderbird"]

--
-- layout hooks
--

myLayoutHook = smartBorders $ avoidStruts $ coreLayoutHook

coreLayoutHook = tiled ||| Mirror tiled ||| Full ||| Grid
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   =  ResizableTall nmaster delta ratio []
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

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
-- main function (no configuration stored there)
--

main :: IO ()
main = do
  session <- getEnv "DESKTOP_SESSION"
  xmonad $ (defDesktopConfig session)
    { modMask     = myModMask
    , borderWidth = myBorderWidth
    , layoutHook  = myLayoutHook
    , manageHook  = myManageHook <+> manageHook (defDesktopConfig session)
    } `additionalKeys` myKeys
