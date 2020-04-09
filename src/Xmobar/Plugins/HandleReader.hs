-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.HandleReader
-- Copyright   :  (c) Pavan Rikhi
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Pavan Rikhi <pavan.rikhi@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
--
-- A plugin for reading from 'Handle's
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.HandleReader
    ( HandleReader(..)
    )
where

import           System.IO                      ( Handle
                                                , hIsEOF
                                                )

import           Xmobar.Run.Exec                ( Exec(..) )
import           Xmobar.System.Utils            ( hGetLineSafe )


-- | A HandleReader displays any text received from a Handle.
--
-- This is only useful if you are running @xmobar@ from other Haskell code.
-- You can create a pair of @(read, write)@ 'Handle's using
-- 'System.Process.createPipe'. Pass the @read@ 'Handle' to HandleReader
-- and write your desired output to the @write@ 'Handle'.
--
-- @
--  (readHandle, writeHandle) <- 'System.Process.createPipe'
--  xmobarProcess <- 'System.Posix.Process.forkProcess' $ 'Xmobar.xmobar' myConfig
--          { commands =
--              'Xmobar.Run' ('HandleReader' readHandle "handle") : 'Xmobar.commands' myConfig
--          }
--  'System.IO.hPutStr' writeHandle "Hello World"
-- @
data HandleReader
    = HandleReader
        Handle
        -- ^ The Handle to read from.
        String
        -- ^ Alias for the HandleReader
    deriving (Show)

-- | WARNING: This Read instance will throw an exception if used! It is
-- only implemented because it is required to use HandleReader with
-- 'Xmobar.Run' in 'Xmobar.commands'.
instance Read HandleReader where
    -- | Throws an 'error'!
    readsPrec = error "HandleReader: Read instance is stub"

-- | Asynchronously read from the 'Handle'.
instance Exec HandleReader where
    -- | Read from the 'Handle' until it is closed.
    start (HandleReader handle _) cb =
        untilM (hIsEOF handle) $ hGetLineSafe handle >>= cb
    -- | Use the 2nd argument to HandleReader as its alias.
    alias (HandleReader _ a) = a

-- Loop the action until predicateM returns True.
untilM :: Monad m => m Bool -> m () -> m ()
untilM predicateM action = do
    predicate <- predicateM
    if predicate then return () else action >> untilM predicateM action
