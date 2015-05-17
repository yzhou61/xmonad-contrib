{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Font
-- Copyright   :  (c) 2007 Andrea Rossato and Spencer Janssen
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for abstracting a font facility over Core fonts and Xft
--
-----------------------------------------------------------------------------

module XMonad.Util.Font
    ( -- * Usage:
      -- $usage
      XMonadFont(..)
    , initXMF
    , releaseXMF
    , initCoreFont
    , releaseCoreFont
    , initUtf8Font
    , releaseUtf8Font
    , Align (..)
    , stringPosition
    , textWidthXMF
    , textExtentsXMF
    , printStringXMF
    , stringToPixel
    , fi
    ) where

import XMonad
import Foreign
import Control.Applicative
import Control.Exception as E
import Data.Maybe

#ifdef XFT
import Data.List
import Graphics.X11.Xft
import Graphics.X11.Xrender
import Data.Char (ord)
#endif

-- Hide the Core Font/Xft switching here
data XMonadFont = Core FontStruct
                | Utf8 FontSet
#ifdef XFT
                | Xft  [XftFont]
#endif

-- $usage
-- See "XMonad.Layout.Tabbed" or "XMonad.Prompt" for usage examples

-- | Get the Pixel value for a named color: if an invalid name is
-- given the black pixel will be returned.
stringToPixel :: (Functor m, MonadIO m) => Display -> String -> m Pixel
stringToPixel d s = fromMaybe fallBack <$> io getIt
    where getIt    = initColor d s
          fallBack = blackPixel d (defaultScreen d)

econst :: a -> IOException -> a
econst = const

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initCoreFont :: String -> X FontStruct
initCoreFont s = do
  d <- asks display
  io $ E.catch (getIt d) (fallBack d)
      where getIt    d = loadQueryFont d s
            fallBack d = econst $ loadQueryFont d "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

releaseCoreFont :: FontStruct -> X ()
releaseCoreFont fs = do
  d <- asks display
  io $ freeFont d fs

initUtf8Font :: String -> X FontSet
initUtf8Font s = do
  d <- asks display
  (_,_,fs) <- io $ E.catch (getIt d) (fallBack d)
  return fs
      where getIt    d = createFontSet d s
            fallBack d = econst $ createFontSet d "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

releaseUtf8Font :: FontSet -> X ()
releaseUtf8Font fs = do
  d <- asks display
  io $ freeFontSet d fs

-- | When initXMF gets a font name that starts with 'xft:' it switches to the Xft backend
-- Example: 'xft: Sans-10'
initXMF :: String -> X XMonadFont
initXMF s =
#ifdef XFT
  if xftPrefix `isPrefixOf` s then
     do dpy <- asks display
        xftdraw <- mapM (openFont dpy) (wordsWhen (==',') (drop (length xftPrefix) s))
        return (Xft xftdraw)
  else
#endif
      fmap Utf8 $ initUtf8Font s
#ifdef XFT
  where xftPrefix = "xft:"
        openFont dpy fontName = do
          xftdraw <- io $ xftFontOpen dpy (defaultScreenOfDisplay dpy) fontName
          return xftdraw
        wordsWhen p string =  case dropWhile p string of
                      "" -> []
                      string' -> w : wordsWhen p string''
                            where (w, string'') = break p string'
#endif

releaseXMF :: XMonadFont -> X ()
#ifdef XFT
releaseXMF (Xft xftfonts) = do
  dpy <- asks display
  mapM_ (closeFont dpy) xftfonts
  where closeFont dpy font = do
          io $ xftFontClose dpy font
#endif
releaseXMF (Utf8 fs) = releaseUtf8Font fs
releaseXMF (Core fs) = releaseCoreFont fs

#ifdef XFT
xftTxtExtents' :: Display -> [XftFont] -> String -> IO XGlyphInfo
xftTxtExtents' d fs string = do
    chunks <- getChunks d fs string
    let (_, _, gi, _, _) = last chunks
    return gi

xftfont_ascent' :: [XftFont] -> IO Int
xftfont_ascent' = (fmap maximum) . (mapM xftfont_ascent)

xftfont_descent' :: [XftFont] -> IO Int
xftfont_descent' = (fmap maximum) . (mapM xftfont_descent)

#endif

textWidthXMF :: MonadIO m => Display -> XMonadFont -> String -> m Int
textWidthXMF _   (Utf8 fs) s = return $ fi $ wcTextEscapement fs s
textWidthXMF _   (Core fs) s = return $ fi $ textWidth fs s
#ifdef XFT
textWidthXMF dpy (Xft xftdraw) s = liftIO $ do
    gi <- xftTxtExtents' dpy xftdraw s
    return $ xglyphinfo_xOff gi
#endif

textExtentsXMF :: MonadIO m => XMonadFont -> String -> m (Int32,Int32)
textExtentsXMF (Utf8 fs) s = do
  let (_,rl)  = wcTextExtents fs s
      ascent  = fi $ - (rect_y rl)
      descent = fi $ rect_height rl + (fi $ rect_y rl)
  return (ascent, descent)
textExtentsXMF (Core fs) s = do
  let (_,a,d,_) = textExtents fs s
  return (a,d)
#ifdef XFT
textExtentsXMF (Xft xftfont) _ = io $ do
  ascent  <- fi `fmap` xftfont_ascent' xftfont
  descent <- fi `fmap` xftfont_descent' xftfont
  return (ascent, descent)
#endif

-- | String position
data Align = AlignCenter | AlignRight | AlignLeft | AlignRightOffset Int
                deriving (Show, Read)

-- | Return the string x and y 'Position' in a 'Rectangle', given a
-- 'FontStruct' and the 'Align'ment
stringPosition :: (Functor m, MonadIO m) => Display -> XMonadFont -> Rectangle -> Align -> String -> m (Position,Position)
stringPosition dpy fs (Rectangle _ _ w h) al s = do
  width <- textWidthXMF dpy fs s
  (a,d) <- textExtentsXMF fs s
  let y = fi $ ((h - fi (a + d)) `div` 2) + fi a;
      x = case al of
            AlignCenter -> fi (w `div` 2) - fi (width `div` 2)
            AlignLeft   -> 1
            AlignRight  -> fi (w - (fi width + 1));
            AlignRightOffset offset -> fi (w - (fi width + 1)) - fi offset;
  return (x,y)

printStringXMF :: (Functor m, MonadIO m) => Display -> Drawable -> XMonadFont -> GC -> String -> String
            -> Position -> Position -> String  -> m ()
printStringXMF d p (Core fs) gc fc bc x y s = io $ do
    setFont d gc $ fontFromFontStruct fs
    [fc',bc'] <- mapM (stringToPixel d) [fc,bc]
    setForeground d gc fc'
    setBackground d gc bc'
    drawImageString d p gc x y s
printStringXMF d p (Utf8 fs) gc fc bc x y s = io $ do
    [fc',bc'] <- mapM (stringToPixel d) [fc,bc]
    setForeground d gc fc'
    setBackground d gc bc'
    io $ wcDrawImageString d p fs gc x y s
#ifdef XFT
printStringXMF dpy drw fs@(Xft font) gc fc bc x y s = do
  let screen   = defaultScreenOfDisplay dpy
      colormap = defaultColormapOfScreen screen
      visual   = defaultVisualOfScreen screen
  bcolor <- stringToPixel dpy bc
  (a,d)  <- textExtentsXMF fs s
  gi <- io $ xftTxtExtents' dpy font s
  io $ setForeground dpy gc bcolor
  io $ fillRectangle dpy drw gc (x - fi (xglyphinfo_x gi))
                                (y - fi a)
                                (fi $ xglyphinfo_xOff gi)
                                (fi $ a + d)
  io $ withXftDraw dpy drw visual colormap $
         \draw -> withXftColorName dpy visual colormap fc $
                   \color -> xftDrawString' draw color font (toInteger x) (toInteger y) s
#endif

-- | Short-hand for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

#ifdef XFT
xftDrawString' :: XftDraw ->
                  XftColor ->
                  [XftFont] ->
                  Integer ->
                  Integer ->
                  String -> IO ()
xftDrawString' d c fs x y string = do
    dpy <- xftDrawDisplay d
    chunks <- getChunks dpy fs string
    mapM_ (\(f, s, _, xo, yo) -> xftDrawString d c f (x+xo) (y+yo) s) chunks

xftCharExists' :: Display -> XftFont -> Char -> IO Bool
xftCharExists' d f c = bool `fmap` xftCharExists d f (fi $ ord c)
  where
    bool 0 = False
    bool _ = True
 
-- Split string and determine fonts/offsets for individual parts
getChunks :: Display -> [XftFont] -> [Char] ->
             IO [(XftFont, String, XGlyphInfo, Integer, Integer)]
getChunks disp fts str = do
    chunks <- getFonts disp fts str
    getOffsets (XGlyphInfo 0 0 0 0 0 0) chunks
  where
    -- Split string and determine fonts for individual parts
    getFonts _ [] _ = return []
    getFonts _ _ [] = return []
    getFonts _ [ft] s = return [(ft, s)]
    getFonts d fonts@(ft:_) s = do
        -- Determine which glyph can be rendered by current font
        glyphs <- mapM (xftCharExists' d ft) s
        -- Split string into parts that can/cannot be rendered
        let splits = split (runs glyphs) s
        -- Determine which font to render each chunk with
        concat `fmap` mapM (getFont d fonts) splits

    -- Determine fonts for substrings
    getFont _ [] _ = return []
    getFont _ [ft] (_, s) = return [(ft, s)] -- Last font, use it
    getFont _ (ft:_) (True, s) = return [(ft, s)] -- Current font can render this substring
    getFont d (_:fs) (False, s) = getFonts d fs s -- Fallback to next font

    -- Helpers
    runs [] = []
    runs (x:xs) = let (h, t) = span (==x) xs in (x, length h + 1) : runs t
    split [] _ = []
    split ((x, c):xs) s = let (h, t) = splitAt c s in (x, h) : split xs t

    -- Determine coordinates for chunks using extents
    getOffsets _ [] = return []
    getOffsets (XGlyphInfo _ _ x y xo yo) ((f, s):chunks) = do
        (XGlyphInfo w' h' _ _ xo' yo') <- xftTextExtents disp f s
        let gi = XGlyphInfo (xo+w') (yo+h') x y (xo+xo') (yo+yo')
        rest <- getOffsets gi chunks
        return $ (f, s, gi, fromIntegral xo, fromIntegral yo) : rest
#endif
