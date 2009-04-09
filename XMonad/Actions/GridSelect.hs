{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.GridSelect
-- Copyright   :  Clemens Fruhwirth <clemens@endorphin.org>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Clemens Fruhwirth <clemens@endorphin.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- GridSelect displays a 2D grid of windows to navigate with cursor
-- keys and to select with return.
--
-----------------------------------------------------------------------------

module XMonad.Actions.GridSelect (
    -- * Usage
    -- $usage
    GSConfig(..),
    defaultGSConfig,
    defaultGSSpawnConfig,
    buildDefaultGSConfig,
    gridselect,
    gridselectWindow,
    withSelectedWindow,
    bringSelected,
    goToSelected,
    spawnSelected,
    fromClassName,
    defaultColorizer,
    colorRangeFromClassName
    ) where
import Data.Maybe
import Data.Bits
import Control.Monad.State
import Control.Arrow
import Data.List as L
import XMonad
import XMonad.Util.Font
import XMonad.Prompt (mkUnmanagedWindow)
import XMonad.StackSet as W
import XMonad.Layout.Decoration
import XMonad.Util.NamedWindows
import XMonad.Actions.WindowBringer (bringWindow)
import Text.Printf
import System.Random (mkStdGen, genRange, next)
import Data.Word (Word8)

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.GridSelect
--
-- Then add a keybinding, e.g.
--
-- >    , ((modMask x, xK_g), goToSelected defaultGSConfig)
--
-- Screenshot: <http://clemens.endorphin.org/gridselect.png>
--
-- This module also supports displaying arbitrary information in a grid and letting
-- the user select from it. E.g. to spawn an application from a given list, you
-- can use the following:
--
-- >   , ((modMask x, xK_s), spawnSelected defaultGSSpawnConfig ["xterm","gmplayer","gvim"])

data GSConfig a = GSConfig {
      gs_cellheight :: Integer,
      gs_cellwidth :: Integer,
      gs_cellpadding :: Integer,
      gs_colorizer :: a -> Bool -> X (String, String),
      gs_font :: String
}

type TwoDPosition = (Integer, Integer)

type TwoDElementMap a = [(TwoDPosition,(String,a))]

data TwoDState a = TwoDState { td_curpos :: TwoDPosition
                             , td_elementmap :: TwoDElementMap a
                             , td_gsconfig :: GSConfig a
                             , td_font :: XMonadFont
                             , td_paneX :: Integer
                             , td_paneY :: Integer
                             , td_drawingWin :: Window
                             }

type TwoD a b = StateT (TwoDState a) X b

diamondLayer :: (Enum b', Num b') => b' -> [(b', b')]
-- FIXME remove nub
diamondLayer n = let ul = [ (x,n-x) | x <- [0..n] ]
        in nub $ ul ++ (map (negate *** id) ul) ++
           (map (negate *** negate) ul) ++
           (map (id *** negate) ul)

diamond :: (Enum a, Num a) => [(a, a)]
diamond = concatMap diamondLayer [0..]

diamondRestrict :: (Enum t, Num t, Ord t) => t -> t -> [(t, t)]
diamondRestrict x y = L.filter (\(x',y') -> abs x' <= x && abs y' <= y) .
                      L.takeWhile (\(x',y') -> abs x' + abs y' <= x+y) $ diamond

tupadd :: (Num t1, Num t) => (t, t1) -> (t, t1) -> (t, t1)
tupadd (a,b) (c,d) = (a+c,b+d)

findInElementMap :: (Eq a) => a -> [(a, b)] -> Maybe (a, b)
findInElementMap pos = find ((== pos) . fst)

drawWinBox :: Window -> XMonadFont -> (String, String) -> Integer -> Integer -> String -> Integer -> Integer -> Integer -> X ()
drawWinBox win font (fg,bg) ch cw text x y cp =
  withDisplay $ \dpy -> do
  gc <- liftIO $ createGC dpy win
  bordergc <- liftIO $ createGC dpy win
  liftIO $ do
    Just fgcolor <- initColor dpy fg
    Just bgcolor <- initColor dpy bg
    Just bordercolor <- initColor dpy borderColor
    setForeground dpy gc fgcolor
    setBackground dpy gc bgcolor
    setForeground dpy bordergc bordercolor
    fillRectangle dpy win gc (fromInteger x) (fromInteger y) (fromInteger cw) (fromInteger ch)
    drawRectangle dpy win bordergc (fromInteger x) (fromInteger y) (fromInteger cw) (fromInteger ch)
  stext <- shrinkWhile (shrinkIt shrinkText)
           (\n -> do size <- liftIO $ textWidthXMF dpy font n
                     return $ size > (fromInteger (cw-(2*cp))))
           text
  printStringXMF dpy win font gc bg fg (fromInteger (x+cp)) (fromInteger (y+(div ch 2))) stext
  liftIO $ freeGC dpy gc
  liftIO $ freeGC dpy bordergc

updateAllElements :: TwoD a ()
updateAllElements =
    do
      TwoDState { td_elementmap = els } <- get
      updateElements els

updateElements :: TwoDElementMap a -> TwoD a ()
updateElements elementmap = do
    TwoDState { td_curpos = curpos,
                td_drawingWin = win,
                td_gsconfig = gsconfig,
                td_font = font,
                td_paneX = paneX,
                td_paneY = paneY} <- get
    let cellwidth = gs_cellwidth gsconfig
        cellheight = gs_cellheight gsconfig
        paneX' = div (paneX-cellwidth) 2
        paneY' = div (paneY-cellheight) 2
        updateElement (pos@(x,y),(text, element)) = lift $ do
            colors <- (gs_colorizer gsconfig) element (pos == curpos)
            drawWinBox win font
                       colors
                       cellheight
                       cellwidth
                       text
                       (paneX'+x*cellwidth)
                       (paneY'+y*cellheight)
                       (gs_cellpadding gsconfig)
    mapM updateElement elementmap
    return ()

eventLoop :: TwoD a (Maybe a)
eventLoop = do
  (keysym,string,event) <- lift $ withDisplay $ \d -> liftIO $ allocaXEvent $ \e -> do
                             nextEvent d e
                             ev <- getEvent e
                             (ks,s) <- if ev_event_type ev == keyPress
                                       then lookupString $ asKeyEvent e
                                       else return (Nothing, "")
                             return (ks,s,ev)
  handle (fromMaybe xK_VoidSymbol keysym,string) event

handle :: (KeySym, String)
       -> Event
       -> StateT (TwoDState a) X (Maybe a)
handle (ks,_) (KeyEvent {ev_event_type = t})
    | t == keyPress && ks == xK_Escape = return Nothing
    | t == keyPress && (ks == xK_Left || ks == xK_h)  = diffAndRefresh (-1,0)
    | t == keyPress && (ks == xK_Right || ks == xK_l) = diffAndRefresh (1,0)
    | t == keyPress && (ks == xK_Down || ks == xK_j) = diffAndRefresh (0,1)
    | t == keyPress && (ks == xK_Up || ks == xK_k) = diffAndRefresh (0,-1)
    | t == keyPress && ks == xK_Return = do
       (TwoDState { td_curpos = pos, td_elementmap = elmap }) <- get
       return $ fmap (snd . snd) $ findInElementMap pos elmap
  where diffAndRefresh diff = do
          state <- get
          let elmap = td_elementmap state
              oldPos = td_curpos state
              newPos = oldPos `tupadd` diff
              newSelectedEl = findInElementMap newPos elmap
          when (isJust newSelectedEl) $ do
                                put state { td_curpos =  newPos }
                                updateElements (catMaybes [(findInElementMap oldPos elmap), newSelectedEl])
          eventLoop

handle _ (ExposeEvent { }) = updateAllElements >> eventLoop

handle _ _ = eventLoop

-- FIXME probably move that into Utils?
-- Conversion scheme as in http://en.wikipedia.org/wiki/HSV_color_space
hsv2rgb :: Fractional a => (Integer,a,a) -> (a,a,a)
hsv2rgb (h,s,v) =
    let hi = (div h 60) `mod` 6 :: Integer
        f = (((fromInteger h)/60) - (fromInteger hi)) :: Fractional a => a
        q = v * (1-f)
        p = v * (1-s)
        t = v * (1-(1-f)*s)
    in case hi of
         0 -> (v,t,p)
         1 -> (q,v,p)
         2 -> (p,v,t)
         3 -> (p,q,v)
         4 -> (t,p,v)
         5 -> (v,p,q)
         _ -> error "The world is ending. x mod a >= a."

-- | Default colorizer for Strings
defaultColorizer :: String -> Bool -> X (String, String)
defaultColorizer s active = 
    let seed x = toInteger (sum $ map ((*x).fromEnum) s) :: Integer
        (r,g,b) = hsv2rgb ((seed 83) `mod` 360,
                           (fromInteger ((seed 191) `mod` 1000))/2500+0.4,
                           (fromInteger ((seed 121) `mod` 1000))/2500+0.4)
    in if active
         then return ("#faff69", "black")
         else return ("#" ++ concat (map (twodigitHex.(round :: Double -> Word8).(*256)) [r, g, b] ), "white")

-- | Colorize a window depending on it's className.
fromClassName :: Window -> Bool -> X (String, String)
fromClassName w active = runQuery className w >>= flip defaultColorizer active

twodigitHex :: Word8 -> String
twodigitHex a = printf "%02x" a

-- | A colorizer that picks a color inside a range,
-- and depending on the window's class.
colorRangeFromClassName :: (Word8, Word8, Word8) -- ^ Beginning of the color range
                        -> (Word8, Word8, Word8) -- ^ End of the color range
                        -> (Word8, Word8, Word8) -- ^ Background of the active window
                        -> (Word8, Word8, Word8) -- ^ Inactive text color
                        -> (Word8, Word8, Word8) -- ^ Active text color
                        -> Window -> Bool -> X (String, String)
colorRangeFromClassName startC endC activeC inactiveT activeT w active = 
    do classname <- runQuery className w
       if active 
         then return (rgbToHex activeC, rgbToHex activeT)
         else return (rgbToHex $ mix startC endC 
                  $ stringToRatio classname, rgbToHex inactiveT)
    where rgbToHex :: (Word8, Word8, Word8) -> String
          rgbToHex (r, g, b) = '#':twodigitHex r
                               ++twodigitHex g++twodigitHex b

-- | Creates a mix of two colors according to a ratio 
-- (1 -> first color, 0 -> second color).
mix :: (Word8, Word8, Word8) -> (Word8, Word8, Word8)
        -> Double -> (Word8, Word8, Word8)
mix (r1, g1, b1) (r2, g2, b2) r = (mix' r1 r2, mix' g1 g2, mix' b1 b2)
    where  mix' a b = truncate $ (fi a * r) + (fi b * (1 - r))

-- | Generates a Double from a string, trying to
-- achieve a random distribution.
-- We create a random seed from the sum of all characters
-- in the string, and use it to generate a ratio between 0 and 1
stringToRatio :: String -> Double
stringToRatio "" = 0
stringToRatio s = let gen = mkStdGen $ sum $ map fromEnum s
                      range = (\(a, b) -> b - a) $ genRange gen
                      randomInt = foldr1 combine $ replicate 20 next
                      combine f1 f2 g = let (_, g') = f1 g in f2 g'
                  in fi (fst $ randomInt gen) / fi range

-- | Brings up a 2D grid of elements in the center of the screen, and one can
-- select an element with cursors keys. The selected element is returned.
gridselect :: forall a . GSConfig a -> [(String,a)] -> X (Maybe a)
gridselect gsconfig elmap =
 withDisplay $ \dpy -> do
    rootw <- asks theRoot
    s <- gets $ screenRect . W.screenDetail . W.current . windowset
    win <- liftIO $ mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw
                    (rect_x s) (rect_y s) (rect_width s) (rect_height s)
    liftIO $ mapWindow dpy win
    liftIO $ selectInput dpy win (exposureMask .|. keyPressMask)
    status <- io $ grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
    font <- initXMF (gs_font gsconfig)
    let screenWidth = toInteger $ rect_width s;
        screenHeight = toInteger $ rect_height s;
    selectedElement <- if (status == grabSuccess) then
                          do
                            let restriction :: Integer -> (GSConfig a -> Integer) -> Double
                                restriction ss cs = ((fromInteger ss)/(fromInteger $ cs gsconfig)-1)/2
                                restrictX = floor $ restriction screenWidth gs_cellwidth
                                restrictY = floor $ restriction screenHeight gs_cellheight
                                elmap' = zip (diamondRestrict restrictX restrictY) elmap
                            selectedElement <- evalStateT (updateAllElements >> eventLoop)
                                                          (TwoDState (0,0)
                                                           elmap'
                                                           gsconfig
                                                           font
                                                           screenWidth
                                                           screenHeight
                                                           win)
                            return selectedElement
                      else
                          return Nothing
    liftIO $ do
      unmapWindow dpy win
      destroyWindow dpy win
      sync dpy False
    releaseXMF font
    return selectedElement

-- | Like `gridSelect' but with the current windows and their titles as elements
gridselectWindow :: GSConfig Window -> X (Maybe Window)
gridselectWindow gsconf = windowMap >>= gridselect gsconf

-- | Brings up a 2D grid of windows in the center of the screen, and one can
-- select a window with cursors keys. The selected window is then passed to
-- a callback function.
withSelectedWindow :: (Window -> X ()) -> GSConfig Window -> X ()
withSelectedWindow callback conf = do
    mbWindow <- gridselectWindow conf
    case mbWindow of
        Just w -> callback w
        Nothing -> return ()

windowMap :: X [(String,Window)]
windowMap = do
    ws <- gets windowset
    wins <- mapM keyValuePair (W.allWindows ws)
    return wins
 where keyValuePair w = flip (,) w `fmap` decorateName' w

decorateName' :: Window -> X String
decorateName' w = do
  fmap show $ getName w

defaultGSConfig :: GSConfig Window
defaultGSConfig = buildDefaultGSConfig fromClassName

-- | Builds a default gs config from a colorizer function.
buildDefaultGSConfig :: (a -> Bool -> X (String,String)) -> GSConfig a
buildDefaultGSConfig col = GSConfig 50 130 10 col "xft:Sans-8"

borderColor :: String
borderColor = "white"

-- | Brings selected window to the current workspace.
bringSelected :: GSConfig Window -> X ()
bringSelected = withSelectedWindow $ \w -> do
    windows (bringWindow w)
    XMonad.focus w
    windows W.shiftMaster

-- | Switches to selected window's workspace and focuses that window.
goToSelected :: GSConfig Window -> X ()
goToSelected = withSelectedWindow $ windows . W.focusWindow

defaultGSSpawnConfig :: GSConfig String
defaultGSSpawnConfig = buildDefaultGSConfig defaultColorizer

-- | Select an application to spawn from a given list
spawnSelected :: GSConfig String -> [String] -> X ()
spawnSelected conf lst = gridselect conf (zip lst lst) >>= flip whenJust spawn

