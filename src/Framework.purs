module Framework
  ( Game, Scene, Event(..), runGame
  , Random, randomInt, randomBool, randomFloat, shuffle
  , Spritesheet, loadSpritesheet
  , Sprite, sprite
  ) where

import Imports

import Effect.Random as Eff
import Effect.Ref as Ref
import FRP.Event (subscribe) as FRP
import FRP.Event.Mouse (down, getMouse, withPosition) as FRP
import Graphics.Canvas as GC


----
-- API
----


type Game state action =
  { init :: Random state
  , render :: state -> Scene action
  , step :: Event action -> state -> Random state
  , viewportSpec ::
    { canvasId :: String
    , width :: Int
    , height :: Int
    }
  }


type Scene action =
  { sprites :: List (Sprite action)
  -- TODO: background(s)
  -- TODO: viewport, with scrolling
  -- TODO: heads-up display (windows, pop-ups, menus, conky) in HTML & CSS
  -- TODO: window title
  }


data Event action
  = Action action
  -- TODO: clock ticks
  -- TODO: key events


derive instance functorEvent :: Functor Event


newtype Random a = Random (Effect a)
derive newtype instance functorRandom :: Functor Random
derive newtype instance applyRandom :: Apply Random
derive newtype instance applicativeRandom :: Applicative Random
derive newtype instance bindRandom :: Bind Random
derive newtype instance monadRandom :: Monad Random


randomFloat :: Number -> Number -> Random Number
randomFloat lo hi = Random do
  t <- Eff.random
  pure $ (1.0 - t)*lo + t*hi


randomInt :: Int -> Int -> Random Int
randomInt lo hi = Random $ Eff.randomInt lo hi


randomBool :: { trueProb :: Number } -> Random Boolean
randomBool { trueProb } = do
  t <- randomFloat 0.0 1.0
  pure $ t <= trueProb


foreign import unsafeFisherYates :: forall a. Array a -> Array a


shuffle :: forall a. List a -> Random (List a)
shuffle xs = pure $ mkList $ unsafeFisherYates $ mkArray xs


-- TODO: more random effects


newtype Spritesheet = Spritesheet GC.CanvasImageSource


data Sprite a = Sprite Spritesheet Region Position a


derive instance functorSprite :: Functor Sprite


sprite :: forall a.
  Spritesheet ->
  { xOffset :: Int
  , yOffset :: Int
  -- , cells :: List { xOffset :: Int, yOffset :: Int } -- TODO: animation
  -- , repeat :: Bool -- TODO: animation
  , width :: Int
  , height :: Int
  } ->
  { x :: Int
  , y :: Int
  , onClick :: a
  -- , onClick2 :: a -- TODO: support left clicking
  -- , onHover :: a -- TODO: support event on hover
  -- , onAnimationEnd :: a -- TODO: support event on animation end
  } ->
  Sprite a
sprite spritesheet { xOffset, yOffset, width, height } { x, y, onClick } =
  Sprite
    spritesheet
    (Region (Position xOffset yOffset) (Span width height))
    (Position x y)
    onClick


loadSpritesheet :: String -> (Spritesheet -> Effect Unit) -> Effect Unit
loadSpritesheet filename cont =
  withSpritesheet
    filename
    (log $ "Failed to load spritesheet: " <> filename)
    (cont <<< Spritesheet)


runGame :: forall state action. Game state action -> Effect Unit
runGame { init, render, step, viewportSpec: { canvasId, width, height } } =
  withCanvas canvasId (Span width height) (log "Failed to init canvas.") \cvs -> do
    initState <- runRandom init
    let initScene = render initState

    stateRef <- Ref.new initState
    sceneRef <- Ref.new initScene

    let
      draw { sprites } = do
        clear cvs
        for_ sprites \(Sprite (Spritesheet sheet) src@(Region _ size) pos _) -> do
          drawSprite sheet src (Region pos size) cvs

      runStep event = do
        oldState <- Ref.read stateRef
        newState <- runRandom $ step event oldState
        let newScene@{ sprites } = render newState
        Ref.write newState stateRef
        Ref.write newScene sceneRef
        draw newScene

    mouse <- FRP.withPosition <$> FRP.getMouse <*> pure FRP.down
    void $ FRP.subscribe mouse \{ pos } -> pos # foldMap \{ x, y } -> do
      { sprites } <- Ref.read sceneRef
      sprites # foldMap
        \(Sprite _ (Region _ (Span dx dy)) (Position x0 y0) action) ->
          when (x0 <= x && x <= x0 + dx && y0 <= y && y <= y0 + dy) $
            runStep (Action action)

    draw initScene


----
-- Private
----


runRandom :: Random ~> Effect
runRandom (Random eff) = eff


foreign import setImageSmoothing :: GC.Context2D -> Boolean -> Effect Unit


newtype Canvas = Canvas
  { context :: GC.Context2D
  , width :: Int
  , height :: Int
  }


data Position = Position Int Int
data Span = Span Int Int
data Region = Region Position Span

type Cont a = (a -> Effect Unit) -> Effect Unit


withSpritesheet :: String -> Effect Unit -> Cont GC.CanvasImageSource
withSpritesheet spritesheetPath onError onLoad =
  GC.tryLoadImage spritesheetPath $ case _ of
    Nothing -> onError
    Just x -> onLoad x


withCanvas :: String -> Span -> Effect Unit -> Cont Canvas
withCanvas canvasId (Span width height) onError onLoad =
  maybe onError onLoad =<< runMaybeT do
    canvas <- MaybeT $ GC.getCanvasElementById canvasId
    lift $ GC.setCanvasDimensions canvas
      { width: toNumber width
      , height: toNumber height
      }
    context <- lift $ GC.getContext2D canvas
    lift $ setImageSmoothing context false
    lift $ GC.setFont context "Comic Sans"
    pure $ Canvas { context, width, height }


drawSprite :: GC.CanvasImageSource -> Region -> Region -> Canvas -> Effect Unit
drawSprite spritesheet
           (Region (Position x0 y0) (Span dx0 dy0))
           (Region (Position x1 y1) (Span dx1 dy1))
           (Canvas { context }) =
  GC.drawImageFull
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
  GC.setFillStyle context "black"
  GC.fillRect context
    { x: toNumber x
    , y: toNumber y
    , width: toNumber dx
    , height: toNumber dy
    }
