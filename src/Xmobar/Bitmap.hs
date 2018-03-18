{-# LANGUAGE CPP, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Bitmap
-- Copyright   :  (C) 2013, 2015, 2017, 2018 Alexander Polakov
-- License     :  BSD3
--
-- Maintainer  :  jao@gnu.org
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Xmobar.Bitmap
 ( updateCache
 , drawBitmap
 , Bitmap(..)) where

import Control.Monad
import Control.Monad.Trans(MonadIO(..))
import Data.Map hiding (map, filter)
import Graphics.X11.Xlib
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Mem.Weak ( addFinalizer )
import Xmobar.ColorCache
import Xmobar.Parsers (Widget(..))
import Xmobar.Actions (Action)

#ifdef XPM
import Xmobar.XPMFile(readXPMFile)
import Control.Applicative((<|>))
#endif

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except(MonadError(..), runExceptT)

#else
import Control.Monad.Error(MonadError(..))
import Control.Monad.Trans.Error(ErrorT, runErrorT)

runExceptT :: ErrorT e m a -> m (Either e a)
runExceptT = runErrorT

#endif

data BitmapType = Mono Pixel | Poly

data Bitmap = Bitmap { width  :: Dimension
                     , height :: Dimension
                     , pixmap :: Pixmap
                     , shapePixmap :: Maybe Pixmap
                     , bitmapType :: BitmapType
                     }

updateCache :: Display -> Window -> Map FilePath Bitmap -> FilePath ->
               [[(Widget, String, Int, Maybe [Action])]] -> IO (Map FilePath Bitmap)
updateCache dpy win cache iconRoot ps = do
  let paths = map (\(Icon p, _, _, _) -> p) . concatMap (filter icons) $ ps
      icons (Icon _, _, _, _) = True
      icons _ = False
      expandPath path@('/':_) = path
      expandPath path@('.':'/':_) = path
      expandPath path@('.':'.':'/':_) = path
      expandPath path = iconRoot </> path
      go m path = if member path m
                     then return m
                     else do bitmap <- loadBitmap dpy win $ expandPath path
                             return $ maybe m (\b -> insert path b m) bitmap
  foldM go cache paths

readBitmapFile'
    :: (MonadError String m, MonadIO m)
    => Display
    -> Drawable
    -> String
    -> m (Dimension, Dimension, Pixmap)
readBitmapFile' d w p = do
   res <- liftIO $ readBitmapFile d w p
   case res of
    Left err -> throwError err
    Right (bw, bh, bp, _, _) -> return (bw, bh, bp)

loadBitmap :: Display -> Drawable -> FilePath -> IO (Maybe Bitmap)
loadBitmap d w p = do
    exist <- doesFileExist p
    if exist
       then do
#ifdef XPM
            res <- runExceptT (tryXBM <|> tryXPM)
#else
            res <- runExceptT tryXBM
#endif
            case res of
                 Right b -> return $ Just b
                 Left err -> do
                     putStrLn err
                     return Nothing
       else
           return Nothing
 where tryXBM = do
           (bw, bh, bp) <- readBitmapFile' d w p
           liftIO $ addFinalizer bp (freePixmap d bp)
           return $ Bitmap bw bh bp Nothing (Mono 1)
#ifdef XPM
       tryXPM = do
           (bw, bh, bp, mbpm) <- readXPMFile d w p
           liftIO $ addFinalizer bp (freePixmap d bp)
           case mbpm of
                Nothing -> return ()
                Just bpm -> liftIO $ addFinalizer bpm (freePixmap d bpm)
           return $ Bitmap bw bh bp mbpm Poly
#endif

drawBitmap :: Display -> Drawable -> GC -> String -> String
              -> Position -> Position -> Bitmap -> IO ()
drawBitmap d p gc fc bc x y i =
    withColors d [fc, bc] $ \[fc', bc'] -> do
    let w = width i
        h = height i
        y' = 1 + y - fromIntegral h `div` 2
    setForeground d gc fc'
    setBackground d gc bc'
    case shapePixmap i of
         Nothing -> return ()
         Just mask -> setClipOrigin d gc x y' >> setClipMask d gc mask
    case bitmapType i of
         Poly -> copyArea d (pixmap i) p gc 0 0 w h x y'
         Mono pl -> copyPlane d (pixmap i) p gc 0 0 w h x y' pl
    setClipMask d gc 0
