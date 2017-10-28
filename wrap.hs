{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE FlexibleContexts #-}

import Data.IORef
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.SVG as SVG
import           Graphics.UI.Threepenny.Core hiding (set, element, children)
import qualified Graphics.UI.Threepenny.Core as THREE (set, children)
import Control.Lens hiding ((#))
import qualified Control.Lens as L ((#))
import Data.List (intersperse)

data Model = Model { _front :: [String], _back :: [String] } deriving Show
makeLenses ''Model

main = startGUI defaultConfig setup

infixl 8 #++
(#++) :: UI Element -> [Element] -> UI Element
a #++ xs = a #+ map UI.element xs

infixl 8 ##
(##) :: (MonadIO m, Widget w) => w -> (m Element -> b) -> b
e ## f = UI.element e # f

s = THREE.set

setup :: Window -> UI ()
setup w = do
    model <- liftIO $ newIORef $ Model [""] [""]
    b <- getBody w

    --on UI.keydown b $ fdown model out1 out2
    on my_keypress b $ fpress model b

fpress :: IORef Model -> Element -> Char -> UI Element
fpress model b c = do
  m <- liftIO $ readIORef model
  let m' = over front (appendChar c) m
  liftIO $ writeIORef model m'
  draw m' b

appendChar :: Char -> [String] -> [String]
appendChar ' ' ss = ss ++ [""]
appendChar c   ss = init ss ++ [last ss ++ [c]]

draw :: Model -> Element -> UI Element
draw m b = do
  ch <- sequence $
      (intersperse spacer $ map string $ m ^. front) ++ [bar]
  b ## s THREE.children ch

spacer :: UI Element
spacer = UI.span # s style [("padding-left", "6px")]

bar :: UI Element
bar = string "|" # s style [("color", "red")]

fdown :: IORef Model -> Element -> Element -> Int -> UI Element
fdown model el1 el2 k = do
  m <- liftIO $ readIORef model
  let a = "foo"
  let b = "foo"
  let (a', b') =
        case k of
          8 ->  if null a then (a,b) else (init a, b)  -- backspace
          36 -> ([], a ++ b)  -- home  -- http://keycode.info/
          35 -> (a ++ b, [])  -- end
          37 -> if null a then (a,b) else (init a, last a : b)  -- left arrow
          39 -> if null b then (a,b) else (a ++ [head b], tail b)  -- right arrow
          46 -> if null b then (a,b) else (a, tail b)  -- delete
          _ -> (a, b)
  liftIO $ writeIORef model m
  el1 ## s text a'
  el2 ## s text b'

-- | Key pressed while element has focus.
my_keypress :: Element -> Event Char
my_keypress = fmap (toEnum . read . head . unsafeFromJSON) . domEvent "keypress"
