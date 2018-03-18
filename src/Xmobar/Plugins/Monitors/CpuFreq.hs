-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.CpuFreq
-- Copyright   :  (c) Juraj Hercek
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Juraj Hercek <juhe_haskell@hck.sk>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A cpu frequency monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.CpuFreq where

import Xmobar.Plugins.Monitors.Common
import Xmobar.Plugins.Monitors.CoreCommon

-- |
-- Cpu frequency default configuration. Default template contains only
-- one core frequency, user should specify custom template in order to
-- get more cpu frequencies.
cpuFreqConfig :: IO MConfig
cpuFreqConfig =
  mkMConfig "Freq: <cpu0>" (map ((++) "cpu" . show) [0 :: Int ..])


-- |
-- Function retrieves monitor string holding the cpu frequency (or
-- frequencies)
runCpuFreq :: [String] -> Monitor String
runCpuFreq _ = do
  suffix <- getConfigValue useSuffix
  ddigits <- getConfigValue decDigits
  let path = ["/sys/devices/system/cpu/cpu", "/cpufreq/scaling_cur_freq"]
      divisor = 1e6 :: Double
      fmt x | x < 1 = if suffix then mhzFmt x ++ "MHz"
                                else ghzFmt x
            | otherwise = ghzFmt x ++ if suffix then "GHz" else ""
      mhzFmt x = show (round (x * 1000) :: Integer)
      ghzFmt = showDigits ddigits
  failureMessage <- getConfigValue naString
  checkedDataRetrieval failureMessage [path] Nothing (/divisor) fmt
