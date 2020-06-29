module Framework
  ( Game, Scene, Event(..), Key(..), runGame
  , Random, randomInt, randomBool, randomFloat, shuffle
  , Spritesheet, loadSpritesheet
  , Sprite, Color(..), Font(..), TextAlign(..), sprite, rectangle
  ) where

import Imports

import Effect.Random as Eff
import Effect.Ref as Ref
import FRP.Event (subscribe) as Event
import FRP.Event.Mouse (getMouse, up, withPosition) as Mouse
import FRP.Event.Keyboard (getKeyboard, down, withKeys) as Keyboard
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


data Key
  = Key_Escape
  | Key_F1
  | Key_F2
  | Key_F3
  | Key_F4
  | Key_F5
  | Key_F6
  | Key_F7
  | Key_F8
  | Key_F9
  | Key_F10
  | Key_F11
  | Key_F12
  | Key_Insert
  | Key_Delete
  | Key_Home
  | Key_End
  | Key_PageUp
  | Key_PageDown
  | Key_UpArrow
  | Key_LeftArrow
  | Key_DownArrow
  | Key_RightArrow
  | Key_Tab
  | Key_CapsLock
  | Key_Shift
  | Key_Control
  | Key_Alt
  | Key_Backspace
  | Key_Enter
  | Key_Space
  | Key_Backtick
  | Key_Hyphen
  | Key_Equals
  | Key_LeftBracket
  | Key_RightBracket
  | Key_Backslash
  | Key_Semicolon
  | Key_Apostrophe
  | Key_Comma
  | Key_Period
  | Key_Slash
  | Key_1
  | Key_2
  | Key_3
  | Key_4
  | Key_5
  | Key_6
  | Key_7
  | Key_8
  | Key_9
  | Key_0
  | Key_Q
  | Key_W
  | Key_E
  | Key_R
  | Key_T
  | Key_Y
  | Key_U
  | Key_I
  | Key_O
  | Key_P
  | Key_A
  | Key_S
  | Key_D
  | Key_F
  | Key_G
  | Key_H
  | Key_J
  | Key_K
  | Key_L
  | Key_Z
  | Key_X
  | Key_C
  | Key_V
  | Key_B
  | Key_N
  | Key_M


data Event action
  = Action action
  | KeyDown Key
  | ClockTick Int


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


shuffle :: forall a. List a -> Random (List a)
shuffle xs = pure $ mkList $ unsafeFisherYates $ mkArray xs


-- TODO: more random effects


newtype Spritesheet = Spritesheet GC.CanvasImageSource


data Sprite a = Sprite (Maybe (Box /\ a)) (Canvas -> Effect Unit)


derive instance functorSprite :: Functor Sprite


newtype Color = Color String
newtype TextAlign = TextAlign String
newtype Font = Font (Int /\ String)
newtype TextSize = TextSize Int


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


-- TODO: This function is probably the wrong approach.
--   I probably want to implement HTML overlay instead
--   of inserting text elements into the Canvas.
text :: forall a.
  { textAlign :: Maybe TextAlign
  , font :: Maybe Font
  , fill :: Maybe Color
  } ->
  String ->
  { x :: Int
  , y :: Int
  , onClick :: Maybe a
  } ->
  Sprite a
text { textAlign, font, fill }
     msg
     { x, y, onClick } =
  Sprite clickAction canvasAction

  where

  toGcTextAlign (TextAlign align) = undefined align

  toGcFont (Font (size /\ face)) = undefined size face

  { dx, dy } = undefined GC.measureText

  clickAction =
    onClick # map \action -> { x, y, dx, dy } /\ action

  canvasAction =
    \(Canvas { context }) -> do
      withLocal (GC.textAlign context)
                (GC.setTextAlign context)
                (map toGcTextAlign textAlign) $ do
        withLocal (GC.font context)
                  (GC.setFont context)
                  (map toGcFont font) $ do
          GC.fillText context msg (toNumber x) (toNumber y)


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

    keys <- Keyboard.withKeys <$> Keyboard.getKeyboard <*> pure Keyboard.down
    void $ Event.subscribe keys \{ value } ->
      case value of
        "Escape" -> Just Key_Escape
        "F1" -> Just Key_F1
        "F2" -> Just Key_F2
        "F3" -> Just Key_F3
        "F4" -> Just Key_F4
        "F5" -> Just Key_F5
        "F6" -> Just Key_F6
        "F7" -> Just Key_F7
        "F8" -> Just Key_F8
        "F9" -> Just Key_F9
        "F10" -> Just Key_F10
        "F11" -> Just Key_F11
        "F12" -> Just Key_F12
        "Insert" -> Just Key_Insert
        "Delete" -> Just Key_Delete
        "Home" -> Just Key_Home
        "End" -> Just Key_End
        "PageUp" -> Just Key_PageUp
        "PageDown" -> Just Key_PageDown
        "ArrowUp" -> Just Key_UpArrow
        "ArrowLeft" -> Just Key_LeftArrow
        "ArrowDown" -> Just Key_DownArrow
        "ArrowRight" -> Just Key_RightArrow
        "Tab" -> Just Key_Tab
        "CapsLock" -> Just Key_CapsLock
        "Shift" -> Just Key_Shift
        "Control" -> Just Key_Control
        "Alt" -> Just Key_Alt
        "Backspace" -> Just Key_Backspace
        "Enter" -> Just Key_Enter
        " " -> Just Key_Space
        "`" -> Just Key_Backtick
        "~" -> Just Key_Backtick
        "-" -> Just Key_Hyphen
        "_" -> Just Key_Hyphen
        " =" -> Just Key_Equals
        "+" -> Just Key_Equals
        "[" -> Just Key_LeftBracket
        "{" -> Just Key_LeftBracket
        "]" -> Just Key_RightBracket
        "}" -> Just Key_RightBracket
        "\\" -> Just Key_Backslash
        "|" -> Just Key_Backslash
        ";" -> Just Key_Semicolon
        ":" -> Just Key_Semicolon
        "'" -> Just Key_Apostrophe
        "\"" -> Just Key_Apostrophe
        "," -> Just Key_Comma
        "<" -> Just Key_Comma
        "." -> Just Key_Period
        ">" -> Just Key_Period
        "/" -> Just Key_Slash
        "?" -> Just Key_Slash
        "1" -> Just Key_1
        "!" -> Just Key_1
        "2" -> Just Key_2
        "@" -> Just Key_2
        "3" -> Just Key_3
        "#" -> Just Key_3
        "4" -> Just Key_4
        "$" -> Just Key_4
        "5" -> Just Key_5
        "%" -> Just Key_5
        "6" -> Just Key_6
        "^" -> Just Key_6
        "7" -> Just Key_7
        "&" -> Just Key_7
        "8" -> Just Key_8
        "*" -> Just Key_8
        "9" -> Just Key_9
        "(" -> Just Key_9
        "0" -> Just Key_0
        ")" -> Just Key_0
        "q" -> Just Key_Q
        "Q" -> Just Key_Q
        "w" -> Just Key_W
        "W" -> Just Key_W
        "e" -> Just Key_E
        "E" -> Just Key_E
        "r" -> Just Key_R
        "R" -> Just Key_R
        "t" -> Just Key_T
        "T" -> Just Key_T
        "y" -> Just Key_Y
        "Y" -> Just Key_Y
        "u" -> Just Key_U
        "U" -> Just Key_U
        "i" -> Just Key_I
        "I" -> Just Key_I
        "o" -> Just Key_O
        "O" -> Just Key_O
        "p" -> Just Key_P
        "P" -> Just Key_P
        "a" -> Just Key_A
        "A" -> Just Key_A
        "s" -> Just Key_S
        "S" -> Just Key_S
        "d" -> Just Key_D
        "D" -> Just Key_D
        "f" -> Just Key_F
        "F" -> Just Key_F
        "g" -> Just Key_G
        "G" -> Just Key_G
        "h" -> Just Key_H
        "H" -> Just Key_H
        "j" -> Just Key_J
        "J" -> Just Key_J
        "k" -> Just Key_K
        "K" -> Just Key_K
        "l" -> Just Key_L
        "L" -> Just Key_L
        "z" -> Just Key_Z
        "Z" -> Just Key_Z
        "x" -> Just Key_X
        "X" -> Just Key_X
        "c" -> Just Key_C
        "C" -> Just Key_C
        "v" -> Just Key_V
        "V" -> Just Key_V
        "b" -> Just Key_B
        "B" -> Just Key_B
        "n" -> Just Key_N
        "N" -> Just Key_N
        "m" -> Just Key_M
        "M" -> Just Key_M
        _ -> Nothing
      # foldMap (runStep <<< KeyDown)

    mouse <- Mouse.withPosition <$> Mouse.getMouse <*> pure Mouse.up
    void $ Event.subscribe mouse \{ pos } -> pos # foldMap \{ x, y } -> do
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


withLocal :: forall m a b. Monad m =>
  m a -> (a -> m Unit) -> Maybe a -> m b -> m b
withLocal _ _ Nothing f = f
withLocal save set (Just x) f = do
  x0 <- save
  set x
  y <- f
  set x0
  pure y


foreign import setImageSmoothing :: GC.Context2D -> Boolean -> Effect Unit


foreign import unsafeFisherYates :: forall a. Array a -> Array a
