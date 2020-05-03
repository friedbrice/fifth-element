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
  col <- randomInt 0 39
  row <- randomInt 0 24
  drawSpriteToGrid ctx
    (Sprite { offsetX: 20, offsetY: 20 })
    (Cell { col, row })
  clickspots # modify_ \spots ->
   let
     spot = Clickspot
       { x: col*32
       , y: row*32
       , width: 32
       , height: 32
       , action: do
           clickspots # write mempty
           drawEnterpriseClickableSpriteToGrid clickspots ctx
       }
   in
     spot:spots


main :: Effect Unit
main = withSpritesheet
  { spritesheetPath: "sprites.png"
  , onError: log "Couldn't load spritesheet!"
  , onLoad: runMain
  }


runMain :: Spritesheet -> Effect Unit
runMain spritesheet = unsafePartial do
  Just ctx <- initCanvas
    { canvasId: "game"
    , spritesheet
    }
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
            x <= pos'.x && pos'.x <= x + width &&
            y <= pos'.y && pos'.y <= y + height
        case clickspot of
          Nothing -> pure unit
          Just cs -> (unwrap cs).action
  drawEnterpriseClickableSpriteToGrid clickspots ctx
  log "🍝"


-- type Scene a = { props :: List (Graphic a) }


-- data Event' a = Action a


-- data Graphic a
-- instance functorGraphic :: Functor Graphic where map = undefined
-- instance applyGraphic :: Apply Graphic where apply = undefined
-- instance applicativeGraphic :: Applicative Graphic where pure = undefined
-- instance bindGraphic :: Bind Graphic where bind = undefined
-- instance monadGraphic :: Monad Graphic


-- data Random a
-- instance functorRandom :: Functor Random where map = undefined
-- instance applyRandom :: Apply Random where apply = undefined
-- instance applicativeRandom :: Applicative Random where pure = undefined
-- instance bindRandom :: Bind Random where bind = undefined
-- instance monadRandom :: Monad Random


-- type Game state action =
--   { init :: Random state
--   , step :: Event' action -> state -> Random state
--   , render :: state -> Scene action
--   }


-- runGame :: forall state action. Game state action -> Effect Unit
-- runGame = undefined


-- randomInt' :: Int -> Int -> Random Int
-- randomInt' = undefined


-- cow :: forall action.
--   { x :: Int
--   , y :: Int
--   , onClick :: action
--   } ->
--   Graphic action
-- cow = undefined


-- cowClicker :: Game { x :: Int, y :: Int } Unit
-- cowClicker =
--   { init:
--     pure { x: 5, y: 3 }

--   , step:
--     \(Action _) _ -> do
--       x <- randomInt' 0 1279
--       y <- randomInt' 0 719
--       pure { x, y }

--   , render:
--     \{ x, y } ->
--       { props: mkList [ cow { x, y, onClick: unit } ] }
--   }
