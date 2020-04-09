{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar
-- Copyright   :  (c) 2011, 2012, 2013, 2014, 2015, 2017, 2018, 2019 Jose Antonio Ortega Ruiz
--                (c) 2007 Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Public interface of the xmobar library
--
-----------------------------------------------------------------------------

module Xmobar (xmobar
              , xmobarMain
              , defaultConfig
              , configFromArgs
              , tenthSeconds
              , Runnable (..)
              , Exec (..)
              , Command (..)
              , module Xmobar.Config.Types
              , module Xmobar.Config.Parse
              , module Xmobar.Plugins.BufferedPipeReader
              , module Xmobar.Plugins.CommandReader
              , module Xmobar.Plugins.Date
#ifdef DATEZONE
              , module Xmobar.Plugins.DateZone
#endif
              , module Xmobar.Plugins.EWMH
              , module Xmobar.Plugins.HandleReader
              , module Xmobar.Plugins.Kbd
              , module Xmobar.Plugins.Locks
#ifdef INOTIFY
              , module Xmobar.Plugins.Mail
              , module Xmobar.Plugins.MBox
#endif
              , module Xmobar.Plugins.Monitors
              , module Xmobar.Plugins.PipeReader
              , module Xmobar.Plugins.MarqueePipeReader
              , module Xmobar.Plugins.StdinReader
              , module Xmobar.Plugins.XMonadLog
              ) where

import Xmobar.Run.Runnable
import Xmobar.Run.Exec
import Xmobar.Run.Command
import Xmobar.Config.Types
import Xmobar.Config.Parse
import Xmobar.Plugins.BufferedPipeReader
import Xmobar.Plugins.CommandReader
import Xmobar.Plugins.Date
#ifdef DATEZONE
import Xmobar.Plugins.DateZone
#endif
import Xmobar.Plugins.EWMH
import Xmobar.Plugins.HandleReader
import Xmobar.Plugins.Kbd
import Xmobar.Plugins.Locks
#ifdef INOTIFY
import Xmobar.Plugins.Mail
import Xmobar.Plugins.MBox
#endif
import Xmobar.Plugins.Monitors
import Xmobar.Plugins.PipeReader
import Xmobar.Plugins.StdinReader
import Xmobar.Plugins.MarqueePipeReader
import Xmobar.Plugins.XMonadLog

import Xmobar.App.Main(xmobar, xmobarMain, configFromArgs)
import Xmobar.App.Config(defaultConfig)
