module Framework
  ( Game, Scene, Event(..), runGame
  , Random, randomInt, randomBool, randomFloat, shuffle
  , Spritesheet, loadSpritesheet
  , Sprite, Color(..), sprite, rectangle
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


data Sprite a = Sprite (Maybe (Box /\ a)) (Canvas -> Effect Unit)


derive instance functorSprite :: Functor Sprite


newtype Color = Color String


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
  , onClick :: Maybe a
  -- , onClick2 :: Maybe a -- TODO: support left clicking
  -- , onHover :: Maybe a -- TODO: support event on hover
  -- , onAnimationEnd :: Maybe a -- TODO: support event on animation end
  } ->
  Sprite a
sprite (Spritesheet spritesheet)
       { xOffset, yOffset, width, height }
       { x, y, onClick } =
  Sprite
    (onClick # map \action -> { x, y, dx: width, dy: height } /\ action)
    \(Canvas { context }) ->
      GC.drawImageFull
        context
        spritesheet
        (toNumber xOffset)
        (toNumber yOffset)
        (toNumber width)
        (toNumber height)
        (toNumber x)
        (toNumber y)
        (toNumber width)
        (toNumber height)


rectangle :: forall a.
  { border :: Maybe (Int /\ Color)
  , fill :: Maybe Color
  , x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  , onClick :: Maybe a
  } ->
  Sprite a
rectangle { border, fill, x, y, width: dx, height: dy, onClick } =
  Sprite
    (onClick # map \action -> { x, y, dx, dy } /\ action)
    \canvas -> do
      fill # foldMap (fillBox canvas { x, y, dx, dy })
      border # foldMap (uncurry $ borderBox canvas { x, y, dx, dy })


loadSpritesheet :: String -> (Spritesheet -> Effect Unit) -> Effect Unit
loadSpritesheet filename cont =
  GC.tryLoadImage filename $ case _ of
    Nothing -> (log $ "Failed to load spritesheet: " <> filename)
    Just x -> cont $ Spritesheet x


runGame :: forall state action. Game state action -> Effect Unit
runGame { init, render, step, viewportSpec: { canvasId, width, height } } =
  withCanvas canvasId { width, height } (log "Failed to init canvas.") \cnv -> do
    initState <- runRandom init
    let initScene = render initState

    stateRef <- Ref.new initState
    sceneRef <- Ref.new initScene

    let
      draw { sprites } = do
        clear cnv
        for_ sprites \(Sprite _ drawSprite) -> drawSprite cnv

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
      sprites # foldMap \(Sprite clickbox _) ->
        clickbox # foldMap \({ x: x0, y: y0, dx, dy } /\ action) ->
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


type Box = { x :: Int, y :: Int, dx :: Int, dy :: Int }


withCanvas ::
  String -> { width :: Int, height :: Int } -> Effect Unit ->
  (Canvas -> Effect Unit) -> Effect Unit
withCanvas canvasId { width, height } onError onLoad =
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


clear :: Canvas -> Effect Unit
clear canvas@(Canvas { width, height }) =
  fillBox canvas { x: 0, y: 0, dx: width, dy: height } (Color "black")


fillBox :: Canvas -> Box -> Color -> Effect Unit
fillBox (Canvas { context }) { x, y, dx, dy } (Color color) = do
  GC.setFillStyle context color
  GC.fillRect context
    { x: toNumber x
    , y: toNumber y
    , width: toNumber dx
    , height: toNumber dy
    }


borderBox :: Canvas -> Box -> Int -> Color -> Effect Unit
borderBox (Canvas { context }) { x, y, dx, dy } width (Color color) = do
  GC.setStrokeStyle context color
  GC.setLineWidth context (toNumber width)
  GC.strokeRect context
    { x: toNumber x
    , y: toNumber y
    , width: toNumber dx
    , height: toNumber dy
    }
