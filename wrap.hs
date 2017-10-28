{-# LANGUAGE ScopedTypeVariables #-}

import Data.IORef
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.SVG  as SVG
import Graphics.UI.Threepenny.Core
main = startGUI defaultConfig setup

infixl 8 #++
(#++) :: UI Element -> [Element] -> UI Element
a #++ xs = a #+ map element xs

setup :: Window -> UI ()
setup w = do
    r1 <- liftIO $ newIORef []
    r2 <- liftIO $ newIORef []
    elemCircle <- SVG.circle
        # set SVG.cx "100" # set SVG.cy "2" # set SVG.r "2"
        # set SVG.fill "blue"
    context <- SVG.svg
        # set SVG.width "300"
        # set SVG.height "100" #++ [elemCircle]
    out1  <- UI.span # set text ""
    out2  <- UI.span # set text ""
    bar <- UI.span # set text "|" # set style [("color", "red")]
    sp <- UI.span #. "sp" #++ [out1, bar, out2]
    wrap <- UI.div #. "wrap"
        # set style [("width","300px"),("height","300px"),("border","solid black 1px"),
                     ("position", "absolute")]
        #++ [sp]
    svgWrap <- UI.div #. "svgwrap"
        # set style [("width","300px"),("height","300px"), ("position", "absolute")
                      ]
        #++ [context]
    getBody w #++ [sp, svgWrap]
    b <- getBody w

    --on UI.mousemove wrap $ fmouse out
    on my_keydown b $ fdown r1 r2 out1 out2 elemCircle
    on my_keypress b $ fpress r1 out1 elemCircle

fdown :: IORef [Char] -> IORef [Char] -> Element -> Element -> Element -> Int -> UI Element
fdown r1 r2 el1 el2 circ k = do
  a <- liftIO $ readIORef r1
  b <- liftIO $ readIORef r2
  let (a', b') =
        case k of
          8 ->  if null a then (a,b) else (init a, b)  -- backspace
          36 -> ([], a ++ b)  -- home  -- http://keycode.info/
          35 -> (a ++ b, [])  -- end
          37 -> if null a then (a,b) else (init a, last a : b)  -- left arrow
          39 -> if null b then (a,b) else (a ++ [head b], tail b)  -- right arrow
          46 -> if null b then (a,b) else (a, tail b)  -- delete
          _ -> (a, b)
  liftIO $ writeIORef r1 a'
  liftIO $ writeIORef r2 b'
  element el1 # set text a'
  element el2 # set text b'
  moveBlob circ

fpress :: IORef [Char] -> Element -> Element -> Char -> UI Element
fpress r el circ c = do
  a <- liftIO $ readIORef r
  let a' = a ++ [c]
  liftIO $ writeIORef r a'
  element el # set text a'
  moveBlob circ

moveBlob :: Element -> UI Element
moveBlob circ = do
  (k :: Double) <- callFunction $ ffi "$('.sp').width()"
  element circ # set SVG.cx (show $ k)


-- | Key pressed while element has focus.
my_keypress :: Element -> Event Char
my_keypress = fmap (toEnum . read . head . unsafeFromJSON) . domEvent "keypress"

-- | Key pressed while element has focus.
my_keydown :: Element -> Event UI.KeyCode
my_keydown = fmap unsafeFromJSON . domEvent "keydown"

fmouse :: Element -> (Int,Int) -> UI Element
fmouse el xy = element el # set text ("hello!!! " ++ show xy)





