module Main where

import Imports

import Framework as F

main :: Effect Unit
main = F.runGame spriteClicker2k20

data Point = Point Int Int
data Click = Click Point

derive instance eqPoint :: Eq Point

makeSprite :: Point -> F.Sprite Click
makeSprite p@(Point x y) = F.sprite
  { xOffset: 59*32
  , yOffset: 73*32
  , width: 1*32
  , height: 1*32
  }
  { x, y, onClick: Click p }

randomPoint :: F.Random Point
randomPoint = do
  x <- F.randomInt 0 (1279 - 32)
  y <- F.randomInt 0 (719 - 32)
  pure $ Point x y

spriteClicker2k20 :: F.Game (List Point) Click
spriteClicker2k20 =
  { init: pure <$> randomPoint
  , render
  , step
  }

step :: F.Event Click -> List Point -> F.Random (List Point)
step (F.Action (Click p)) points = do
  newPoints <-
    F.randomInt 1 2
      # map (range 1)
      # flatMap (traverse $ const randomPoint)
  pure $ newPoints <> delete p points

render :: List Point -> F.Scene Click
render points = { sprites: makeSprite <$> points }
