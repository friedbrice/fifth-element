module Framework
  ( Game, Scene, Event(Action), runGame
  , Random, randomInt, randomBool
  , Sprite, sprite
  ) where

import Imports

import Effect.Random as Effect
import Effect.Ref as Ref
import FRP.Event as Event
import FRP.Event.Mouse as Mouse
import Graphics.Canvas as Canvas


----
-- Rendering Primitives
----


foreign import setImageSmoothing :: Canvas.Context2D -> Boolean -> Effect Unit


newtype Canvas = Canvas { context :: Canvas.Context2D, width :: Int, height :: Int }


data Position = Position Int Int
data Span = Span Int Int
data Region = Region Position Span


withSpritesheet ::
  String -> Effect Unit ->
  (Canvas.CanvasImageSource -> Effect Unit) -> Effect Unit
withSpritesheet spritesheetPath onError onLoad =
  Canvas.tryLoadImage spritesheetPath $ case _ of
    Nothing -> onError
    Just x -> onLoad x


withCanvas ::
  String -> Span -> Effect Unit ->
  (Canvas -> Effect Unit) -> Effect Unit
withCanvas canvasId (Span width height) onError onLoad =
  maybe onError onLoad =<< runMaybeT do
    canvas <- MaybeT $ Canvas.getCanvasElementById canvasId
    lift $ Canvas.setCanvasDimensions canvas
      { width: toNumber width
      , height: toNumber height
      }
    context <- lift $ Canvas.getContext2D canvas
    lift $ setImageSmoothing context false
    lift $ Canvas.setFont context "Comic Sans"
    pure $ Canvas { context, width, height }


drawSprite :: Canvas.CanvasImageSource -> Region -> Region -> Canvas -> Effect Unit
drawSprite spritesheet
           (Region (Position x0 y0) (Span dx0 dy0))
           (Region (Position x1 y1) (Span dx1 dy1))
           (Canvas { context }) =
  Canvas.drawImageFull
    context
    spritesheet
    (toNumber x0)
    (toNumber y0)
    (toNumber dx0)
    (toNumber dy0)
    (toNumber x1)
    (toNumber y1)
    (toNumber dx1)
    (toNumber dy1)


clear :: Canvas -> Effect Unit
clear canvas@(Canvas { width, height }) =
  clearRegion (Region (Position 0 0) (Span width height)) canvas


clearRegion :: Region -> Canvas -> Effect Unit
clearRegion (Region (Position x y) (Span dx dy)) (Canvas { context }) = do
  Canvas.setFillStyle context "black"
  Canvas.fillRect context
    { x: toNumber x
    , y: toNumber y
    , width: toNumber dx
    , height: toNumber dy
    }


----
-- API
----


data Event action = Action action
-- instance functorEvent :: Functor Event where map = undefined
-- instance applyEvent :: Apply Event where apply = undefined
-- instance applicativeEvent :: Applicative Event where pure = undefined
-- instance bindEvent :: Bind Event where bind = undefined
-- instance monadEvent :: Monad Event


newtype Random a = Random (Effect a)
derive newtype instance functorRandom :: Functor Random
derive newtype instance applyRandom :: Apply Random
derive newtype instance applicativeRandom :: Applicative Random
derive newtype instance bindRandom :: Bind Random
derive newtype instance monadRandom :: Monad Random


runRandom :: Random ~> Effect
runRandom (Random eff) = eff


randomInt :: Int -> Int -> Random Int
randomInt lo hi = Random $ Effect.randomInt lo hi


randomBool :: Random Boolean
randomBool = Random Effect.randomBool


data Sprite a = Sprite Region Position a
-- instance functorSprite :: Functor Sprite where map = undefined
-- instance applySprite :: Apply Sprite where apply = undefined
-- instance applicativeSprite :: Applicative Sprite where pure = undefined
-- instance bindSprite :: Bind Sprite where bind = undefined
-- instance monadSprite :: Monad Sprite


sprite :: forall a.
  { xOffset :: Int, yOffset :: Int, width :: Int, height :: Int } ->
  { x :: Int, y :: Int, onClick :: a } ->
  Sprite a
sprite { xOffset, yOffset, width, height } { x, y, onClick } =
  Sprite (Region (Position xOffset yOffset) (Span width height)) (Position x y) onClick


type Scene action = { sprites :: List (Sprite action) }


type Game state action =
  { init :: Random state
  , render :: state -> Scene action
  , step :: Event action -> state -> Random state
  }


runGame :: forall state action. Game state action -> Effect Unit
runGame { init, render, step } =
  withSpritesheet "sprites.png" (log "Failed to load spritesheet.") \spritesheet ->
    withCanvas "game" (Span 1280 800) (log "Failed to init canvas.") \canvas -> do
      initState <- runRandom init
      let initScene = render initState

      stateRef <- Ref.new initState
      sceneRef <- Ref.new initScene

      let
        draw { sprites } = do
          clear canvas
          for_ sprites \(Sprite src@(Region _ size) pos _) -> do
            drawSprite spritesheet src (Region pos size) canvas

        runStep event = do
          oldState <- Ref.read stateRef
          newState <- runRandom $ step event oldState
          let newScene@{ sprites } = render newState
          Ref.write newState stateRef
          Ref.write newScene sceneRef
          draw newScene

      mouse <- Mouse.withPosition <$> Mouse.getMouse <*> pure Mouse.down
      void $ Event.subscribe mouse \{ pos } -> pos # foldMap \{ x, y } -> do
        { sprites } <- Ref.read sceneRef
        sprites # foldMap \(Sprite (Region _ (Span dx dy)) (Position x0 y0) action) ->
          when (x0 <= x && x <= x0 + dx && y0 <= y && y <= y0 + dy) $ runStep (Action action)

      draw initScene
