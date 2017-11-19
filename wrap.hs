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
    span1 <- UI.span # T.set style [("font-family", "sans-serif")]
    span2 <- UI.span #. "w" # T.set style [("visibility", "hidden")]

    elemRect <- SVG.rect
        # T.set SVG.x "8" # T.set SVG.y "2" # T.set SVG.width "100"
        # T.set SVG.height "100"
        # T.set SVG.fill "pink"
    context <- SVG.svg
        # T.set SVG.width "300"
        # T.set SVG.height "100" #+ [UI.element elemRect]
    svgWrap <- UI.div #. "svgwrap"
        # T.set style [("width","300px"),("height","300px"), ("position", "absolute"),
                        ("top","0px"), ("left","0px"), ("color", "red"),
                        ("opacity","0.2")]
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
  let bck = map (shift (-4)) $ sp (m ^. back)
  let elems = sp (m ^. front) ++ [bar] ++ bck
  ch <- sequence elems
  widths <- mapM (elWidth span2) ch
  let n = length $ takeWhile (< 100) $ scanl1 (+) widths
  br <- UI.br
  sp20 <- spacer 20
  let ch' = take n ch ++ [br, sp20] ++ drop n ch
  setCh span1 ch'

shift :: Int -> UI Element -> UI Element
shift n = T.set style [("position", "relative"), ("left", show n ++ "px")]

elWidth :: Element -> Element -> UI Double
elWidth b e = do
  setCh b [e]
  callFunction $ ffi "$('.w').width()"

setCh :: Element -> [Element] -> UI Element
setCh e es = UI.element e # T.set T.children es

sp :: String -> [UI Element]
sp = intersperse (spacer 5) . map string . splitOn " "

spacer :: Int -> UI Element
spacer n = UI.span # T.set style [("padding-left", show n ++ "px")]

bar :: UI Element
bar = string "|" # T.set style [("color", "red")] # shift (-2)

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
