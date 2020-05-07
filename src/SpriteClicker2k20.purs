module SpriteClicker2k20 (runSpriteClicker2k20) where

import Imports
import Framework as F

runSpriteClicker2k20 :: Effect Unit
runSpriteClicker2k20 =
  F.loadSpritesheet "sprites.png" \spritesheet ->
    F.runGame $ spriteClicker2k20 spritesheet

data Point = Point Int Int
data Click = Click Point

derive instance eqPoint :: Eq Point

spriteClicker2k20 :: F.Spritesheet -> F.Game (List Point) Click
spriteClicker2k20 spritesheet =
  { viewportSpec:
    { canvasId: "game"
    , width: 1280
    , height: 800
    }
  , init: pure <$> randomPoint
  , render
  , step
  }
  where

  randomPoint :: F.Random Point
  randomPoint = do
    x <- F.randomInt 0 (1279 - 32)
    y <- F.randomInt 0 (719 - 32)
    pure $ Point x y

  render :: List Point -> F.Scene Click
  render points = { sprites: makeSprite <$> points }

  makeSprite :: Point -> F.Sprite Click
  makeSprite p@(Point x y) = F.sprite
    spritesheet
    { xOffset: 59*32
    , yOffset: 73*32
    , width: 1*32
    , height: 1*32
    }
    { x, y, onClick: Just $ Click p }


  step :: F.Event Click -> List Point -> F.Random (List Point)
  step (F.Action (Click p)) points = do
    newPoints <-
      F.randomInt 1 2
        # map (range 1)
        # flatMap (traverse $ const randomPoint)
    pure $ newPoints <> delete p points
