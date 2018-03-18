-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Plugins
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module exports the API for plugins.
--
-- Have a look at Plugins\/HelloWorld.hs
--
-----------------------------------------------------------------------------

module Xmobar.Plugins
    ( Exec (..)
    , tenthSeconds
    , readFileSafe
    , hGetLineSafe
    ) where

import Xmobar.Commands
import Xmobar.XUtil
