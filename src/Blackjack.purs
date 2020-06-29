module Blackjack where

import Imports
import Framework as F

runBlackjack :: Effect Unit
runBlackjack = F.runGame $ blackjack

type State = Unit
type Action = Unit

blackjack :: F.Game State Action
blackjack =
  { viewportSpec:
    { canvasId: "game"
    , width: 48 * 8
    , height: 48 * 8
    }
  , init: initialState
  , render
  , step
  }

initialState :: F.Random State
initialState = pure unit

render :: State -> F.Scene Action
render = undefined

step :: F.Event Action -> State -> F.Random State
step = undefined
