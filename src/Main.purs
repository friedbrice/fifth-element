module Main where

import Imports

import Effect.Console (log)

import Canvas
import Effect.Aff
import Effect.Class
import Partial.Unsafe
import Effect.Ref

import FRP.Event.Mouse
import FRP.Event
import Effect.Random

newtype Clickspot a = Clickspot
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , action :: a
  }

derive instance newtypeClickspot :: Newtype (Clickspot a) _

drawEnterpriseClickableSpriteToGrid
  :: Ref (List (Clickspot (Effect Unit)))
  -> Context
  -> Effect Unit
drawEnterpriseClickableSpriteToGrid clickspots ctx = do
  clear ctx
  x <- randomInt 0 39
  y <- randomInt 0 24
  drawSpriteToGrid ctx
    (Sprite { offsetX: 20, offsetY: 20 })
    (V { x, y })
  clickspots # modify_ \spots ->
   let
     spot = Clickspot
       { x: x*32
       , y: y*32
       , width: 32
       , height: 32
       , action: do
           clickspots # write mempty
           -- recurse
           drawEnterpriseClickableSpriteToGrid clickspots ctx
       }
   in
     spot:spots

main :: Effect Unit
main = unsafePartial $ launchAff_ do
  Just ctx <- initCanvas
    { canvasId: "game"
    , spritesheetPath: "sprites.png"
    }
  liftEffect do
    clear ctx
    clickspots <- (new mempty) :: Effect (Ref (List (Clickspot (Effect Unit))))
    moose <- getMouse
    let mooseClicks = withPosition moose down
    cancelMoose <- subscribe mooseClicks \stuff@{pos, value} -> do
      log $ show stuff
      clickspots' <- read clickspots
      case pos of
        Nothing -> pure unit
        Just pos' -> do
          let
            clickspot = clickspots' # find \(Clickspot {x, y, width, height }) ->
              pos'.x <= x + width && pos'.x >= x &&
              pos'.y <= y + height && pos'.y >= y
          case clickspot of
            Nothing -> pure unit
            Just cs -> (unwrap cs).action
    drawEnterpriseClickableSpriteToGrid clickspots ctx
    log "🍝"
