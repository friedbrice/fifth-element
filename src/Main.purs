module Main where

import Imports

import Framework as F

main :: Effect Unit
main = F.runGame spriteClicker2k20

type Point = { x :: Int, y :: Int }
data Click = Click

tile :: Point -> F.Sprite Click
tile { x, y } = F.sprite
  { xOffset: 1*32
  , yOffset: 0*32
  , width: 1*32
  , height: 1*32
  }
  { x, y, onClick: Click }

randomPosition :: F.Random Point
randomPosition = do
  x <- F.randomInt 0 (1279 - 32)
  y <- F.randomInt 0 (719 - 32)
  pure $ { x, y }

spriteClicker2k20 :: F.Game Point Click
spriteClicker2k20 =
  { init: randomPosition
  , render: \xy -> { sprites: mkList [ tile xy ] }
  , step: \_ _ -> randomPosition
  }
