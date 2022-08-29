--
-- ~/.xmonad/xmonad.hs
--

import System.Posix.Env (getEnv)
import System.IO
import System.Directory
import Data.Maybe (maybe)
import Graphics.X11.ExtraTypes.XF86

import XMonad

import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce

import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns

import XMonad.Actions.PhysicalScreens
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W

--
-- basic configuration
--

myModMask     = mod4Mask -- use the Windows key as mod
myBorderWidth = 2        -- set window border size
myTerminal    = "urxvt256c-ml" -- preferred terminal emulator

--
-- key bindings
--

myKeys = [
   ((myModMask, xK_a), sendMessage MirrorShrink) -- for  ResizableTall
 , ((myModMask, xK_z), sendMessage MirrorExpand) -- for  ResizableTall
 -- selecting particular monitors
 , ((myModMask, xK_w), viewScreen def 0)
 , ((myModMask, xK_e), viewScreen def 1)
 , ((myModMask, xK_r), viewScreen def 2)
 -- cycling through workspaces in multi monitor setup, skipping scratchpad
 , ((myModMask .|. mod5Mask, xK_h), prevHiddenNonEmptyNoSPWS)
 , ((myModMask, xK_Left),           prevHiddenNonEmptyNoSPWS)
 , ((myModMask .|. mod5Mask, xK_l), nextHiddenNonEmptyNoSPWS)
 , ((myModMask, xK_Right),          nextHiddenNonEmptyNoSPWS)
 , ((myModMask, xK_o), scratchPad)
 --((myModMask, xK_d), spawn "/home/martin/bin/qstardict-show-hide.sh")
 ]
 where
   scratchPad = scratchpadSpawnActionTerminal myTerminal
   getSortByIndexNoSP = fmap (.scratchpadFilterOutWorkspace) getSortByIndex
   prevHiddenNonEmptyNoSPWS = windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Prev (hiddenWS :&: Not emptyWS) 1
   nextHiddenNonEmptyNoSPWS = windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Next (hiddenWS :&: Not emptyWS) 1

-- key bindings used only in stand alone mode (without KDE)
myStandAloneKeys = [
   ((myModMask, xK_x),             spawn "xscreensaver-command -lock")
 , ((0, xF86XK_MonBrightnessUp),   spawn "brightnessctl set +5%")
 , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 5%-")
 , ((0, xF86XK_AudioRaiseVolume),  spawn "amixer -D pipewire sset Master 10%+")
 , ((0, xF86XK_AudioLowerVolume),  spawn "amixer -D pipewire sset Master 10%-")
 , ((0, xF86XK_AudioMute),         spawn "amixer -D pipewire sset Master toggle")
 , ((0, xF86XK_AudioMicMute),      spawn "amixer -D pipewire sset Capture toggle")
 ]

--
-- hooks for newly created windows
-- note: run 'xprop WM_CLASS' to get className
--

myManageHook :: ManageHook
myManageHook = manageDocks <+> manageScratchPad <+> coreManageHook

coreManageHook :: ManageHook
coreManageHook = composeAll . concat $
  [ [ className   =? c --> doFloat           | c <- myFloats]
  , [ className   =? c --> doF (W.shift "9") | c <- mailIrcApps]
  ]
  where
    myFloats      = [
       "MPlayer"
     , "Gimp"
     , "Plasma-desktop"
     , "plasmashell"
     , "krunner"
     , "Klipper"
     , "Keepassx"
     , "Starplot"
     ]
    mailIrcApps   = [
       "thunderbird"
     , "konversation"
     , "Mail"
     ]

-- yakuake style hook
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.4     -- terminal height, 40%
    w = 1       -- terminal width, 100%
    t = 1 - h   -- distance from top edge, 90%
    l = 1 - w   -- distance from left edge, 0%

--
-- startup hooks
--

myStartupHook = setWMName "LG3D"

--
-- layout hooks
--

myLayoutHook = smartBorders $ avoidStruts $ coreLayoutHook

coreLayoutHook = tiled ||| Mirror tiled ||| Full ||| tiled3 ||| Grid
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = ResizableTall nmaster delta ratio []
    -- like the tall, but with three columns
    tiled3  = ThreeColMid nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

--
-- log hook (for xmobar)
--

myLogHook xmproc = dynamicLogWithPP xmobarPP
  { ppOutput = hPutStrLn xmproc
  , ppTitle  = xmobarColor "green" "" . shorten 50
  }

--
-- desktop :: DESKTOP_SESSION -> desktop_configuration
--

desktop "gnome"         = gnomeConfig
desktop "xmonad-gnome"  = gnomeConfig
desktop "kde"           = kde4Config
desktop "kde-plasma"    = kde4Config
desktop "plasma"        = kde4Config
desktop "xfce"          = xfceConfig
desktop _               = desktopConfig

--
-- main function (no configuration stored there)
--

main :: IO ()
main = do
  session <- getEnv "DESKTOP_SESSION"
  let defDesktopConfig = maybe desktopConfig desktop session
      myDesktopConfig = defDesktopConfig
        { modMask     = myModMask
        , borderWidth = myBorderWidth
        , startupHook = myStartupHook
        , layoutHook  = myLayoutHook
        , manageHook  = myManageHook <+> manageHook defDesktopConfig
        } `additionalKeys` myKeys
  -- when running standalone (no KDE), try to spawn xmobar (if installed)
  xmobarInstalled <- doesFileExist "/usr/bin/xmobar"
  if session == Just "xmonad" && xmobarInstalled
    then do mproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
            xmonad $ myDesktopConfig
              { logHook  = myLogHook mproc
              , terminal = myTerminal
              } `additionalKeys` myStandAloneKeys
    else do xmonad myDesktopConfig
