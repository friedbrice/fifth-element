module Canvas where

import Imports

-- import Extra.Prelude
--
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)

import Data.String.CodePoints as String
import Effect.Aff (Aff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Data.Array as Array
--
-- import Constants (tileMapDimensions, canvasDimensions, font, black, Color(..), displayDimensions, charWidth, charHeight)
import Graphics.Canvas as Canvas
-- import Types (Sprite (..))
import Data.Int (toNumber)

newtype Context = Context { context :: Canvas.Context2D, spritesheet :: Canvas.CanvasImageSource }

newtype Color = Color String
black = Color "black"

newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }

newtype Vector a = V { x :: a, y :: a }

displayDimensions = { drawWidth: 32, drawHeight: 32, width: 1280, height: 720 }
tileMapDimensions = { width: 32, height: 32, padding: 0 }
canvasDimensions = { height: 800.0, width: 1280.0 }
font = "Comic Sans"

initCanvas :: { canvasId :: String, spritesheetPath :: String } -> Aff (Maybe Context)
initCanvas { canvasId, spritesheetPath } = runMaybeT do
  canvas <- MaybeT $ liftEffect $ Canvas.getCanvasElementById canvasId
  liftEffect $ Canvas.setCanvasDimensions canvas canvasDimensions
  context <- liftEffect $ Canvas.getContext2D canvas
  liftEffect $ setImageSmoothing context false
  liftEffect $ Canvas.setFont context font
  spritesheet <- lift $ makeAff \handler -> do
    Canvas.tryLoadImage spritesheetPath (handler <<< maybe (Left $ error "failed to load image") pure)
    mempty
  pure $ Context { context, spritesheet }

foreign import setImageSmoothing :: Canvas.Context2D -> Boolean -> Effect Unit

drawSprite :: Context -> Sprite -> Vector Number -> Effect Unit
drawSprite (Context {context, spritesheet}) (Sprite { offsetX, offsetY }) (V { x, y }) =
  let
    { width, height, padding } = tileMapDimensions
    { drawWidth, drawHeight} = displayDimensions
    sourceX = toNumber (offsetX * (width + padding))
    sourceY = toNumber (offsetY * (height + padding))
    w = toNumber width
    h = toNumber height
    p = toNumber padding
  in
  Canvas.drawImageFull context spritesheet sourceX sourceY w h x y (toNumber drawWidth) (toNumber drawHeight)

drawSpriteToGrid :: Context -> Sprite -> Vector Int -> Effect Unit
drawSpriteToGrid ctx sprite (V { x, y }) =
  let
    { drawWidth, drawHeight } = displayDimensions
    canvasX = toNumber (x * drawWidth)
    canvasY = toNumber (y * drawHeight)
  in
  when
    ( 0 <= x && x < displayDimensions.width
      && 0 <= y && y < displayDimensions.height
      ) $ drawSprite ctx sprite (V {x: canvasX, y: canvasY})
    -- (Canvas.drawImageFull context spritesheet sourceX sourceY w h canvasX canvasY w h)

-- dotXLoc :: Number
-- dotXLoc = toNumber 517
--
-- dotYLoc :: Number
-- dotYLoc = toNumber 504
--
-- darkDotY :: Number
-- darkDotY = toNumber 402
--
-- dotXOffset :: Int -> Int -> Int -> Number
-- dotXOffset i base width = toNumber $ base + (i - 1) * dotOrigSize * width
--
-- dotHeight :: Number
-- dotHeight = toNumber $ (drawHeight / 8) - 1
--   where
--     { drawWidth, drawHeight } = displayDimensions
--
-- dotOrigSize :: Int
-- dotOrigSize = 2
--
-- drawLightDot :: Context -> Int -> Int -> Int -> Int -> Effect Unit
-- drawLightDot = drawDot dotYLoc
--
-- drawDarkDot :: Context -> Int -> Int -> Int -> Int -> Effect Unit
-- drawDarkDot = drawDot darkDotY
--
-- drawDot :: Number -> Context -> Int -> Int -> Int -> Int -> Effect Unit
-- drawDot yLoc (Context {context, spritesheet}) baseX baseY width i =
--   Canvas.drawImageFull context spritesheet dotXLoc yLoc (toNumber dotOrigSize) (toNumber dotOrigSize) (dotXOffset i baseX width) (toNumber baseY) (toNumber width) dotHeight
--
-- drawGrowthToGrid :: Context -> Int -> Int -> Vector Int -> Effect Unit
-- drawGrowthToGrid ctx@(Context {context, spritesheet}) ndots totalDots (V { x, y } ) =
--   let
--     { drawWidth, drawHeight } = displayDimensions
--     baseX = x * drawWidth + 10
--     baseY = y * drawHeight + 10
--     width = drawWidth / 16
--    in do
--     traverse_ (drawLightDot ctx baseX baseY width) (Array.range 1 ndots)
--     when (ndots < totalDots) $ traverse_ (drawDarkDot ctx baseX baseY width) (Array.range (ndots + 1) totalDots)
--
-- drawDamageToGrid :: Context -> Int -> Vector Int -> Effect Unit
-- drawDamageToGrid ctx@(Context {context, spritesheet}) ndots (V { x, y } ) =
--   let
--     { drawWidth, drawHeight } = displayDimensions
--     baseX = x * drawWidth + 10
--     baseY = y * drawHeight + drawHeight * 7 / 8
--     width = drawWidth / 10
--    in
--     traverse_ (drawLightDot ctx baseX baseY width) (Array.range 1 ndots)
--
-- getTextDimensions :: String -> { width :: Number, height :: Number }
-- getTextDimensions t = { width: charWidth * (toNumber $ String.length t), height: charHeight }

-- drawText :: Context -> Color -> Number -> Number -> String -> Effect Unit
-- drawText ctx@(Context {context}) color x y text = do
--   let {width, height} = getTextDimensions text
--   clearRegion ctx {x,y, width, height}
--   setFillStyle ctx color
--   Canvas.fillText context text (textOffset.x + x) (textOffset.y + y)

-- drawTextToGrid :: Context -> Color -> String -> Vector Int -> Effect Unit
-- drawTextToGrid ctx color text (V {x,y}) = drawText ctx color x' y' text
--   where
--    x' = toNumber x * charWidth
--    y' = toNumber y * charHeight

-- drawLinesToGrid :: Context -> Color -> Vector Int -> Array String -> Effect Unit
-- drawLinesToGrid ctx color (V {x,y}) texts = forWithIndex_  texts \i t ->
--   drawTextToGrid ctx color t (V {x, y: y + i})
--
--
clear :: Context -> Effect Unit
clear ctx@(Context{context}) = do
  setFillStyle ctx black
  Canvas.fillRect context { x: 0.0, y: 0.0, width: canvasDimensions.width, height: canvasDimensions.height }

clearRegion :: Context -> {x :: Number, y :: Number, width :: Number, height :: Number} -> Effect Unit
clearRegion ctx@(Context {context}) rect = do
  setFillStyle ctx black
  Canvas.fillRect context rect

setFillStyle :: Context -> Color -> Effect Unit
setFillStyle (Context {context}) (Color c) = Canvas.setFillStyle context c

-- textOffset :: { x ∷ Number, y ∷ Number }
-- textOffset = { x: 3.0, y: 13.0 }
