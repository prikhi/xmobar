{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XPMFile
-- Copyright   :  (C) 2014 Alexander Shabalin
-- License     :  BSD3
--
-- Maintainer  :  jao@gnu.org
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Xmobar.XPMFile(readXPMFile) where

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except(MonadError(..))
#else
import Control.Monad.Error(MonadError(..))
#endif
import Control.Monad.Trans(MonadIO(..))
import Graphics.X11.Xlib(Dimension, Display(..), Drawable, Pixmap)
import Foreign.C.String(CString, withCString)
import Foreign.C.Types(CInt(..), CLong)
import Foreign.Ptr(Ptr)
import Foreign.Marshal.Alloc(alloca, allocaBytes)
import Foreign.Storable(peek, peekByteOff, pokeByteOff)

#include <X11/xpm.h>

foreign import ccall "XpmReadFileToPixmap"
    xpmReadFileToPixmap :: Display -> Drawable -> CString -> Ptr Pixmap -> Ptr Pixmap -> Ptr () -> IO CInt

readXPMFile
    :: (MonadError String m, MonadIO m)
    => Display
    -> Drawable
    -> String
    -> m (Dimension, Dimension, Pixmap, Maybe Pixmap)
readXPMFile display d filename =
    toError $ withCString filename $ \c_filename ->
    alloca $ \pixmap_return ->
    alloca $ \shapemask_return ->
    allocaBytes (#size XpmAttributes) $ \attributes -> do
        (#poke XpmAttributes, valuemask) attributes ((#const XpmReturnAllocPixels) :: CLong)
        res <- xpmReadFileToPixmap display d c_filename pixmap_return shapemask_return attributes
        case res of
             0 -> do
                 width <- (#peek XpmAttributes, width) attributes
                 height <- (#peek XpmAttributes, height) attributes
                 pixmap <- peek pixmap_return
                 shapemask <- peek shapemask_return
                 return $ Right (width, height, pixmap, if shapemask == 0 then Nothing else Just shapemask)
             1 -> return $ Left "readXPMFile: XpmColorError"
             -1 -> return $ Left "readXPMFile: XpmOpenFailed"
             -2 -> return $ Left "readXPMFile: XpmFileInvalid"
             -3 -> return $ Left "readXPMFile: XpmNoMemory"
             -4 -> return $ Left "readXPMFile: XpmColorFailed"
             _ -> return $ Left "readXPMFile: Unknown error"
    where toError m = either throwError return =<< liftIO m
