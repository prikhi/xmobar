{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.StdinReader
-- Copyright   :  (c) Spencer Janssen
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin to display information from _XMONAD_LOG, specified at
-- http://code.haskell.org/XMonadContrib/XMonad/Hooks/DynamicLog.hs
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.XMonadLog (XMonadLog(..)) where

import Control.Monad
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Xmobar.Plugins
#ifdef UTF8
#undef UTF8
import Codec.Binary.UTF8.String as UTF8
#define UTF8
#endif
import Foreign.C (CChar)
import Xmobar.XUtil (nextEvent')
import Xmobar.Actions (stripActions)

data XMonadLog = XMonadLog
               | UnsafeXMonadLog
               | XPropertyLog String
               | UnsafeXPropertyLog String
               | NamedXPropertyLog String String
               | UnsafeNamedXPropertyLog String String
    deriving (Read, Show)

instance Exec XMonadLog where
    alias XMonadLog = "XMonadLog"
    alias UnsafeXMonadLog = "UnsafeXMonadLog"
    alias (XPropertyLog atom) = atom
    alias (NamedXPropertyLog _ name) = name
    alias (UnsafeXPropertyLog atom) = atom
    alias (UnsafeNamedXPropertyLog _ name) = name

    start x cb = do
        let atom = case x of
                XMonadLog -> "_XMONAD_LOG"
                UnsafeXMonadLog -> "_XMONAD_LOG"
                XPropertyLog a -> a
                UnsafeXPropertyLog a -> a
                NamedXPropertyLog a _ -> a
                UnsafeNamedXPropertyLog a _ -> a
            sanitize = case x of
                UnsafeXMonadLog -> id
                UnsafeXPropertyLog _ -> id
                UnsafeNamedXPropertyLog _ _ -> id
                _ -> stripActions

        d <- openDisplay ""
        xlog <- internAtom d atom False

        root  <- rootWindow d (defaultScreen d)
        selectInput d root propertyChangeMask

        let update = do
                        mwp <- getWindowProperty8 d xlog root
                        maybe (return ()) (cb . sanitize . decodeCChar) mwp

        update

        allocaXEvent $ \ep -> forever $ do
            nextEvent' d ep
            e <- getEvent ep
            case e of
                PropertyEvent { ev_atom = a } | a ==  xlog -> update
                _ -> return ()

        return ()

decodeCChar :: [CChar] -> String
#ifdef UTF8
#undef UTF8
decodeCChar = UTF8.decode . map fromIntegral
#define UTF8
#else
decodeCChar = map (toEnum . fromIntegral)
#endif
