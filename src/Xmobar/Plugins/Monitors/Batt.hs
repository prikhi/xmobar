-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Batt
-- Copyright   :  (c) 2010, 2011, 2012, 2013, 2015, 2016, 2018 Jose A Ortega
--                (c) 2010 Andrea Rossato, Petr Rockai
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A battery monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Batt ( battConfig, runBatt, runBatt' ) where

import Control.Exception (SomeException, handle)
import Xmobar.Plugins.Monitors.Common
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), hGetLine, withFile)
import System.Posix.Files (fileExist)
import System.Console.GetOpt
import Data.List (sort, sortBy, group)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Text.Read (readMaybe)

data BattOpts = BattOpts
  { onString :: String
  , offString :: String
  , idleString :: String
  , posColor :: Maybe String
  , lowWColor :: Maybe String
  , mediumWColor :: Maybe String
  , highWColor :: Maybe String
  , lowThreshold :: Float
  , highThreshold :: Float
  , onlineFile :: FilePath
  , scale :: Float
  , onIconPattern :: Maybe IconPattern
  , offIconPattern :: Maybe IconPattern
  , idleIconPattern :: Maybe IconPattern
  }

defaultOpts :: BattOpts
defaultOpts = BattOpts
  { onString = "On"
  , offString = "Off"
  , idleString = "On"
  , posColor = Nothing
  , lowWColor = Nothing
  , mediumWColor = Nothing
  , highWColor = Nothing
  , lowThreshold = 10
  , highThreshold = 12
  , onlineFile = "AC/online"
  , scale = 1e6
  , onIconPattern = Nothing
  , offIconPattern = Nothing
  , idleIconPattern = Nothing
  }

options :: [OptDescr (BattOpts -> BattOpts)]
options =
  [ Option "O" ["on"] (ReqArg (\x o -> o { onString = x }) "") ""
  , Option "o" ["off"] (ReqArg (\x o -> o { offString = x }) "") ""
  , Option "i" ["idle"] (ReqArg (\x o -> o { idleString = x }) "") ""
  , Option "p" ["positive"] (ReqArg (\x o -> o { posColor = Just x }) "") ""
  , Option "l" ["low"] (ReqArg (\x o -> o { lowWColor = Just x }) "") ""
  , Option "m" ["medium"] (ReqArg (\x o -> o { mediumWColor = Just x }) "") ""
  , Option "h" ["high"] (ReqArg (\x o -> o { highWColor = Just x }) "") ""
  , Option "L" ["lowt"] (ReqArg (\x o -> o { lowThreshold = read x }) "") ""
  , Option "H" ["hight"] (ReqArg (\x o -> o { highThreshold = read x }) "") ""
  , Option "f" ["online"] (ReqArg (\x o -> o { onlineFile = x }) "") ""
  , Option "s" ["scale"] (ReqArg (\x o -> o {scale = read x}) "") ""
  , Option "" ["on-icon-pattern"] (ReqArg (\x o ->
     o { onIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["off-icon-pattern"] (ReqArg (\x o ->
     o { offIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["idle-icon-pattern"] (ReqArg (\x o ->
     o { idleIconPattern = Just $ parseIconPattern x }) "") ""
  ]

parseOpts :: [String] -> IO BattOpts
parseOpts argv =
  case getOpt Permute options argv of
    (o, _, []) -> return $ foldr id defaultOpts o
    (_, _, errs) -> ioError . userError $ concat errs

data Status = Charging | Discharging | Full | Idle | Unknown deriving (Read, Eq)

data Result = Result Float Float Float Status | NA

sysDir :: FilePath
sysDir = "/sys/class/power_supply"

battConfig :: IO MConfig
battConfig = mkMConfig
       "Batt: <watts>, <left>% / <timeleft>" -- template
       ["leftbar", "leftvbar", "left", "acstatus", "timeleft", "watts", "leftipat"] -- replacements

data Files = Files
  { fFull :: String
  , fNow :: String
  , fVoltage :: String
  , fCurrent :: String
  , fStatus :: String
  , isCurrent :: Bool
  } | NoFiles deriving Eq

data Battery = Battery
  { full :: !Float
  , now :: !Float
  , power :: !Float
  , status :: !String
  }

safeFileExist :: String -> String -> IO Bool
safeFileExist d f = handle noErrors $ fileExist (d </> f)
  where noErrors = const (return False) :: SomeException -> IO Bool

batteryFiles :: String -> IO Files
batteryFiles bat =
  do is_charge <- exists "charge_now"
     is_energy <- if is_charge then return False else exists "energy_now"
     is_power <- exists "power_now"
     plain <- exists (if is_charge then "charge_full" else "energy_full")
     let cf = if is_power then "power_now" else "current_now"
         sf = if plain then "" else "_design"
     return $ case (is_charge, is_energy) of
       (True, _) -> files "charge" cf sf is_power
       (_, True) -> files "energy" cf sf is_power
       _ -> NoFiles
  where prefix = sysDir </> bat
        exists = safeFileExist prefix
        files ch cf sf ip = Files { fFull = prefix </> ch ++ "_full" ++ sf
                                  , fNow = prefix </> ch ++ "_now"
                                  , fCurrent = prefix </> cf
                                  , fVoltage = prefix </> "voltage_now"
                                  , fStatus = prefix </> "status"
                                  , isCurrent = not ip}

haveAc :: FilePath -> IO Bool
haveAc f =
  handle onError $ withFile (sysDir </> f) ReadMode (fmap (== "1") . hGetLine)
  where onError = const (return False) :: SomeException -> IO Bool

readBattery :: Float -> Files -> IO Battery
readBattery _ NoFiles = return $ Battery 0 0 0 "Unknown"
readBattery sc files =
    do a <- grab $ fFull files
       b <- grab $ fNow files
       d <- grab $ fCurrent files
       s <- grabs $ fStatus files
       let sc' = if isCurrent files then sc / 10 else sc
           a' = max a b -- sometimes the reported max charge is lower than
       return $ Battery (3600 * a' / sc') -- wattseconds
                        (3600 * b / sc') -- wattseconds
                        (d / sc') -- watts
                        s -- string: Discharging/Charging/Full
    where grab f = handle onError $ withFile f ReadMode (fmap read . hGetLine)
          onError = const (return (-1)) :: SomeException -> IO Float
          grabs f = handle onError' $ withFile f ReadMode hGetLine
          onError' = const (return "Unknown") :: SomeException -> IO String

-- sortOn is only available starting at ghc 7.10
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

mostCommonDef :: Eq a => a -> [a] -> a
mostCommonDef x xs = head $ last $ [x] : sortOn length (group xs)

readBatteries :: BattOpts -> [Files] -> IO Result
readBatteries opts bfs =
    do let bfs' = filter (/= NoFiles) bfs
       bats <- mapM (readBattery (scale opts)) (take 3 bfs')
       ac <- haveAc (onlineFile opts)
       let sign = if ac then 1 else -1
           ft = sum (map full bats)
           left = if ft > 0 then sum (map now bats) / ft else 0
           watts = sign * sum (map power bats)
           time = if watts == 0 then 0 else max 0 (sum $ map time' bats)
           mwatts = if watts == 0 then 1 else sign * watts
           time' b = (if ac then full b - now b else now b) / mwatts
           statuses :: [Status]
           statuses = map (fromMaybe Unknown . readMaybe)
                          (sort (map status bats))
           acst = mostCommonDef Unknown $ filter (Unknown/=) statuses
           racst | acst /= Unknown = acst
                 | time == 0 = Idle
                 | ac = Charging
                 | otherwise = Discharging
       return $ if isNaN left then NA else Result left watts time racst

runBatt :: [String] -> Monitor String
runBatt = runBatt' ["BAT", "BAT0", "BAT1", "BAT2"]

runBatt' :: [String] -> [String] -> Monitor String
runBatt' bfs args = do
  opts <- io $ parseOpts args
  c <- io $ readBatteries opts =<< mapM batteryFiles bfs
  suffix <- getConfigValue useSuffix
  d <- getConfigValue decDigits
  nas <- getConfigValue naString
  case c of
    Result x w t s ->
      do l <- fmtPercent x
         ws <- fmtWatts w opts suffix d
         si <- getIconPattern opts s x
         parseTemplate (l ++ [fmtStatus opts s nas, fmtTime $ floor t, ws, si])
    NA -> getConfigValue naString
  where fmtPercent :: Float -> Monitor [String]
        fmtPercent x = do
          let x' = minimum [1, x]
          p <- showPercentWithColors x'
          b <- showPercentBar (100 * x') x'
          vb <- showVerticalBar (100 * x') x'
          return [b, vb, p]
        fmtWatts x o s d = do
          ws <- showWithPadding $ showDigits d x ++ (if s then "W" else "")
          return $ color x o ws
        fmtTime :: Integer -> String
        fmtTime x = hours ++ ":" ++ if length minutes == 2
                                    then minutes else '0' : minutes
          where hours = show (x `div` 3600)
                minutes = show ((x `mod` 3600) `div` 60)
        fmtStatus opts Idle _ = idleString opts
        fmtStatus _ Unknown na = na
        fmtStatus opts Full _ = idleString opts
        fmtStatus opts Charging _ = onString opts
        fmtStatus opts Discharging _ = offString opts
        maybeColor Nothing str = str
        maybeColor (Just c) str = "<fc=" ++ c ++ ">" ++ str ++ "</fc>"
        color x o | x >= 0 = maybeColor (posColor o)
                  | -x >= highThreshold o = maybeColor (highWColor o)
                  | -x >= lowThreshold o = maybeColor (mediumWColor o)
                  | otherwise = maybeColor (lowWColor o)
        getIconPattern opts st x = do
          let x' = minimum [1, x]
          case st of
               Unknown -> showIconPattern (offIconPattern opts) x'
               Idle -> showIconPattern (idleIconPattern opts) x'
               Full -> showIconPattern (idleIconPattern opts) x'
               Charging -> showIconPattern (onIconPattern opts) x'
               Discharging -> showIconPattern (offIconPattern opts) x'
