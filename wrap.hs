{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE FlexibleContexts #-}

import Data.IORef
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.SVG as SVG
import           Graphics.UI.Threepenny.Core hiding (set, element, children)
import qualified Graphics.UI.Threepenny.Core as T (set, children)
import Control.Lens hiding ((#))
import qualified Control.Lens as L ((#))
import Data.List (intersperse)
import Data.List.Split (splitOn)

data Model = Model { _front :: String, _back :: String } deriving Show
makeLenses ''Model

main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    model <- liftIO $ newIORef $ Model "" ""
    b <- getBody w
    span1 <- UI.span
    span2 <- UI.span #. "w" # T.set style [("visibility", "hidden")]

    elemCircle <- SVG.circle
        # T.set SVG.cx "108" # T.set SVG.cy "2" # T.set SVG.r "2"
        # T.set SVG.fill "blue"
    context <- SVG.svg
        # T.set SVG.width "300"
        # T.set SVG.height "100" #+ [UI.element elemCircle]
    svgWrap <- UI.div #. "svgwrap"
        # T.set style [("width","300px"),("height","300px"), ("position", "absolute"),
                        ("top","0px"), ("left","0px")]
        #+ [UI.element context]

    setCh b [span1, span2, svgWrap]
    on UI.keydown b  $ fdown  model span1 span2
    on my_keypress b $ fpress model span1 span2

fpress :: IORef Model -> Element -> Element -> Char -> UI Element
fpress model span1 span2 c = do
  m <- liftIO $ readIORef model
  let m' = over front (++ [c]) m
  liftIO $ writeIORef model m'
  draw m' span1 span2

draw :: Model -> Element -> Element -> UI Element
draw m span1 span2 = do
  let elems = sp (m ^. front) ++ [bar] ++ sp (m ^. back)
  ch <- sequence elems
  widths <- mapM (elWidth span2) ch
  let n = length $ takeWhile (< 100) $ scanl1 (+) widths
  br <- UI.br
  let ch' = take n ch ++ [br] ++ drop n ch
  setCh span1 ch'

elWidth :: Element -> Element -> UI Double
elWidth b e = do
  setCh b [e]
  callFunction $ ffi "$('.w').width()"

setCh :: Element -> [Element] -> UI Element
setCh e es = UI.element e # T.set T.children es

sp :: String -> [UI Element]
sp = intersperse spacer . map string . splitOn " "

spacer :: UI Element
spacer = UI.span # T.set style [("padding-left", "5px")]

bar :: UI Element
bar = string "|" # T.set style [("color", "red")]

fdown :: IORef Model -> Element -> Element -> Int -> UI Element
fdown model span1 span2 k = do
  m <- liftIO $ readIORef model
  let a = m ^. front
  let b = m ^. back
  let (a', b') =
        case k of
          8 ->  if null a then (a,b) else (init a, b)  -- backspace
          36 -> ([], a ++ b)  -- home  -- http://keycode.info/
          35 -> (a ++ b, [])  -- end
          37 -> if null a then (a,b) else (init a, last a : b)  -- left arrow
          39 -> if null b then (a,b) else (a ++ [head b], tail b)  -- right arrow
          46 -> if null b then (a,b) else (a, tail b)  -- delete
          _ -> (a, b)
  let m' = Model a' b'
  liftIO $ writeIORef model m'
  draw m' span1 span2

-- | Key pressed while element has focus.
my_keypress :: Element -> Event Char
my_keypress = fmap (toEnum . read . head . unsafeFromJSON) . domEvent "keypress"
