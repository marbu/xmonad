--
-- ~/.config/taffybar/taffybar.hs
--

import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.Battery

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU

--
-- pager
--

myPagerConfig :: PagerConfig
myPagerConfig = defaultPagerConfig
  { emptyWorkspace = (\xs -> "")
  }

--
-- monitoring/graphs
--

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

memCfg :: GraphConfig
memCfg = defaultGraphConfig
  { graphDataColors = [(1, 0, 0, 1)]
  , graphLabel = Just "mem"
  }

cpuCallback :: IO [Double]
cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

cpuCfg :: GraphConfig
cpuCfg = defaultGraphConfig
  { graphDataColors = [ (0, 1, 0, 1)
                      , (1, 0, 1, 0.5)
                      ]
  , graphLabel = Just "cpu"
  }

--
-- main function
--

main :: IO ()
main = do
  -- define widgets
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew myPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      mpris = mprisNew defaultMPRISConfig
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 1 cpuCallback
      tray = systrayNew
      battery = batteryBarNew defaultBatteryConfig 10
  -- start taffybar (similar to xmonad)
  defaultTaffybar defaultTaffybarConfig
    { startWidgets = [ pager, note ]
    , endWidgets   = [ tray, clock, battery, mem, cpu, mpris ]
    }
