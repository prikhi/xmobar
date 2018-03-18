-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Locks
-- Copyright   :  (c) Patrick Chilton
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Patrick Chilton <chpatrick@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin that displays the status of the lock keys.
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Locks where

import Graphics.X11
import Data.List
import Data.Bits
import Control.Monad
import Graphics.X11.Xlib.Extras
import Xmobar.Plugins
import Xmobar.Plugins.Kbd
import Xmobar.XUtil (nextEvent')

data Locks = Locks
    deriving (Read, Show)

locks :: [ ( KeySym, String )]
locks = [ ( xK_Caps_Lock,   "CAPS"   )
        , ( xK_Num_Lock,    "NUM"    )
        , ( xK_Scroll_Lock, "SCROLL" )
        ]

run' :: Display -> Window -> IO String
run' d root = do
    modMap <- getModifierMapping d
    ( _, _, _, _, _, _, _, m ) <- queryPointer d root

    ls <- filterM ( \( ks, _ ) -> do
        kc <- keysymToKeycode d ks
        return $ case find (elem kc . snd) modMap of
            Nothing       -> False
            Just ( i, _ ) -> testBit m (fromIntegral i)
        ) locks

    return $ unwords $ map snd ls

instance Exec Locks where
    alias Locks = "locks"
    start Locks cb = do
        d <- openDisplay ""
        root <- rootWindow d (defaultScreen d)
        _ <- xkbSelectEventDetails d xkbUseCoreKbd xkbIndicatorStateNotify m m

        allocaXEvent $ \ep -> forever $ do
            cb =<< run' d root
            nextEvent' d ep
            getEvent ep

        closeDisplay d
        return ()
      where
        m = xkbAllStateComponentsMask
