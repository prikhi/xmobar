-----------------------------------------------------------------------------
-- |
-- Module      :  XUtil
-- Copyright   :  (C) 2011, 2012, 2013, 2014, 2015, 2017 Jose Antonio Ortega Ruiz
--                (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  jao@gnu.org
-- Stability   :  unstable
-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Xmobar.XUtil
    ( XFont
    , initFont
    , initCoreFont
    , initUtf8Font
    , textExtents
    , textWidth
    , printString
    , newWindow
    , nextEvent'
    , readFileSafe
    , hGetLineSafe
    , io
    , fi
    ) where

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Trans
import Control.Exception (SomeException, handle)
import Data.List
import Foreign
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import qualified Graphics.X11.Xlib as Xlib (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import System.Mem.Weak ( addFinalizer )
import System.Posix.Types (Fd(..))
import System.IO

#if defined XFT || defined UTF8
# if __GLASGOW_HASKELL__ < 612
import qualified System.IO.UTF8 as UTF8 (readFile,hGetLine)
# else
import qualified System.IO as UTF8 (readFile,hGetLine)
# endif
#endif
#if defined XFT
import Xmobar.MinXft
import Graphics.X11.Xrender
#endif

import Xmobar.ColorCache

readFileSafe :: FilePath -> IO String
#if defined XFT || defined UTF8
readFileSafe = UTF8.readFile
#else
readFileSafe = readFile
#endif

hGetLineSafe :: Handle -> IO String
#if defined XFT || defined UTF8
hGetLineSafe = UTF8.hGetLine
#else
hGetLineSafe = hGetLine
#endif

-- Hide the Core Font/Xft switching here
data XFont = Core FontStruct
           | Utf8 FontSet
#ifdef XFT
           | Xft  [AXftFont]
#endif

-- | When initFont gets a font name that starts with 'xft:' it switchs
-- to the Xft backend Example: 'xft:Sans-10'
initFont :: Display ->String -> IO XFont
initFont d s =
       let xftPrefix = "xft:" in
       if  xftPrefix `isPrefixOf` s then
#ifdef XFT
           fmap Xft $ initXftFont d s
#else
           do
               hPutStrLn stderr $ "Warning: Xmobar must be built with "
                   ++ "the with_xft flag to support font '" ++ s
                   ++ ".' Falling back on default."
               initFont d miscFixedFont
#endif
       else
#if defined UTF8 ||  __GLASGOW_HASKELL__ >= 612
           fmap Utf8 $ initUtf8Font d s
#else
           fmap Core $ initCoreFont d s
#endif

miscFixedFont :: String
miscFixedFont = "-misc-fixed-*-*-*-*-*-*-*-*-*-*-*-*"

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initCoreFont :: Display -> String -> IO FontStruct
initCoreFont d s = do
  f <- handle fallBack getIt
  addFinalizer f (freeFont d f)
  return f
      where getIt = loadQueryFont d s
            fallBack :: SomeException -> IO FontStruct
            fallBack = const $ loadQueryFont d miscFixedFont

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initUtf8Font :: Display -> String -> IO FontSet
initUtf8Font d s = do
  setupLocale
  (_,_,f) <- handle fallBack getIt
  addFinalizer f (freeFontSet d f)
  return f
      where getIt = createFontSet d s
            fallBack :: SomeException -> IO ([String], String, FontSet)
            fallBack = const $ createFontSet d miscFixedFont

#ifdef XFT
initXftFont :: Display -> String -> IO [AXftFont]
initXftFont d s = do
  setupLocale
  let fontNames = wordsBy (== ',') (drop 4 s)
  mapM openFont fontNames
  where
    openFont fontName = do
        f <- openAXftFont d (defaultScreenOfDisplay d) fontName
        addFinalizer f (closeAXftFont d f)
        return f
    wordsBy p str = case dropWhile p str of
                        ""   -> []
                        str' -> w : wordsBy p str''
                                where
                                    (w, str'') = break p str'
#endif

textWidth :: Display -> XFont -> String -> IO Int
textWidth _   (Utf8 fs) s = return $ fi $ wcTextEscapement fs s
textWidth _   (Core fs) s = return $ fi $ Xlib.textWidth fs s
#ifdef XFT
textWidth dpy (Xft xftdraw) s = do
    gi <- xftTxtExtents' dpy xftdraw s
    return $ xglyphinfo_xOff gi
#endif

textExtents :: XFont -> String -> IO (Int32,Int32)
textExtents (Core fs) s = do
  let (_,a,d,_) = Xlib.textExtents fs s
  return (a,d)
textExtents (Utf8 fs) s = do
  let (_,rl)  = wcTextExtents fs s
      ascent  = fi $ - (rect_y rl)
      descent = fi $ rect_height rl + fi (rect_y rl)
  return (ascent, descent)
#ifdef XFT
textExtents (Xft xftfonts) _ = do
  ascent  <- fi `fmap` xft_ascent'  xftfonts
  descent <- fi `fmap` xft_descent' xftfonts
  return (ascent, descent)
#endif

printString :: Display -> Drawable -> XFont -> GC -> String -> String
            -> Position -> Position -> String -> Int -> IO ()
printString d p (Core fs) gc fc bc x y s a = do
    setFont d gc $ fontFromFontStruct fs
    withColors d [fc, bc] $ \[fc', bc'] -> do
      setForeground d gc fc'
      when (a == 255) (setBackground d gc bc')
      drawImageString d p gc x y s

printString d p (Utf8 fs) gc fc bc x y s a =
    withColors d [fc, bc] $ \[fc', bc'] -> do
      setForeground d gc fc'
      when (a == 255) (setBackground d gc bc')
      io $ wcDrawImageString d p fs gc x y s

#ifdef XFT
printString dpy drw fs@(Xft fonts) _ fc bc x y s al =
  withDrawingColors dpy drw fc bc $ \draw fc' bc' -> do
    when (al == 255) $ do
      (a,d)  <- textExtents fs s
      gi <- xftTxtExtents' dpy fonts s
      drawXftRect draw bc' x (y - a) (1 + xglyphinfo_xOff gi) (a + d + 2)
    drawXftString' draw fc' fonts (toInteger x) (toInteger y) s
#endif


-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
newWindow :: Display -> Screen -> Window -> Rectangle -> Bool -> IO Window
newWindow dpy scr rw (Rectangle x y w h) o = do
  let visual = defaultVisualOfScreen scr
      attrmask = if o then cWOverrideRedirect else 0
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes o
           createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr)
                        inputOutput visual attrmask attributes
-- | A version of nextEvent that does not block in foreign calls.
nextEvent' :: Display -> XEventPtr -> IO ()
nextEvent' d p = do
    pend <- pending d
    if pend /= 0
        then nextEvent d p
        else do
            threadWaitRead (Fd fd)
            nextEvent' d p
 where
    fd = connectionNumber d

io :: MonadIO m => IO a -> m a
io = liftIO

-- | Short-hand for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

#if __GLASGOW_HASKELL__ < 612 && (defined XFT || defined UTF8)
#include <locale.h>
foreign import ccall unsafe "locale.h setlocale"
    setlocale :: CInt -> CString -> IO CString

setupLocale :: IO ()
setupLocale = withCString "" (setlocale $ #const LC_ALL) >> return ()
# else
setupLocale :: IO ()
setupLocale = return ()
#endif
