module Canvas where

import Imports

import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Graphics.Canvas as Canvas
import Data.Int (toNumber)

foreign import setImageSmoothing :: Canvas.Context2D -> Boolean -> Effect Unit


newtype Context = Context
  { context :: Canvas.Context2D
  , spritesheet :: Canvas.CanvasImageSource
  }


type Spritesheet = Canvas.CanvasImageSource
newtype Color = Color String
newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }
newtype Pixel = Pixel { x :: Int, y :: Int }
newtype Span = Span { width :: Int, height :: Int }
newtype Cell = Cell { col :: Int, row :: Int }


displayDimensions ::
  { drawHeight :: Int
  , drawWidth :: Int
  , height :: Int
  , width :: Int
  }
displayDimensions =
  { drawWidth: 32, drawHeight: 32, width: 1280, height: 720 }


spritesheetDimentions ::
  { height :: Int
  , padding :: Int
  , width :: Int
  }
spritesheetDimentions =
  { width: 32, height: 32, padding: 0 }


canvasDimensions ::
  { height :: Int
  , width :: Int
  }
canvasDimensions =
  { width: 1280, height: 800 }


withSpritesheet ::
  { spritesheetPath :: String
  , onLoad :: Spritesheet -> Effect Unit
  , onError :: Effect Unit
  } ->
  Effect Unit
withSpritesheet { spritesheetPath, onLoad, onError } =
  Canvas.tryLoadImage spritesheetPath $ case _ of
    Nothing -> onError
    Just x -> onLoad x


initCanvas ::
  { canvasId :: String
  , spritesheet :: Spritesheet
  } ->
  Effect (Maybe Context)
initCanvas { canvasId, spritesheet } = runMaybeT do
  canvas <- MaybeT $ Canvas.getCanvasElementById canvasId
  lift $ Canvas.setCanvasDimensions canvas
    { width: toNumber canvasDimensions.width
    , height: toNumber canvasDimensions.height
    }
  context <- lift $ Canvas.getContext2D canvas
  lift $ setImageSmoothing context false
  lift $ Canvas.setFont context "Comic Sans"
  pure $ Context { context, spritesheet }


drawSprite :: Context -> Sprite -> Pixel -> Effect Unit
drawSprite (Context { context, spritesheet })
           (Sprite { offsetX, offsetY })
           (Pixel { x, y }) =
  let
    { width, height, padding } = spritesheetDimentions
    { drawWidth, drawHeight } = displayDimensions
    sourceX = toNumber $ offsetX * (width + padding)
    sourceY = toNumber $ offsetY * (height + padding)
  in
  Canvas.drawImageFull
    context
    spritesheet
    sourceX
    sourceY
    (toNumber width)
    (toNumber height)
    (toNumber x)
    (toNumber y)
    (toNumber drawWidth)
    (toNumber drawHeight)


drawSpriteToGrid :: Context -> Sprite -> Cell -> Effect Unit
drawSpriteToGrid ctx sprite (Cell { col, row }) =
  let
    { drawWidth, drawHeight } = displayDimensions
    inBounds =
      0 <= col && col < displayDimensions.width &&
      0 <= row && row < displayDimensions.height
  in
  when inBounds $
    drawSprite ctx sprite (Pixel { x: col * drawWidth, y: row * drawHeight })


clear :: Context -> Effect Unit
clear ctx@(Context { context }) = do
  clearRegion
    ctx
    (Pixel { x: 0, y: 0 })
    (Span { width: canvasDimensions.width, height: canvasDimensions.height })


clearRegion :: Context -> Pixel -> Span -> Effect Unit
clearRegion ctx@(Context { context })
            (Pixel { x, y })
            (Span { width, height }) = do
  Canvas.setFillStyle context "black"
  Canvas.fillRect context
    { x: toNumber x
    , y: toNumber y
    , width: toNumber width
    , height: toNumber height
    }
