module Main where

import Imports
import BudgetChess (runBudgetChess)
import SpriteClicker2k20 (runSpriteClicker2k20)

main :: Effect Unit
main =
  const runSpriteClicker2k20 runBudgetChess
  -- const runBudgetChess runSpriteClicker2k20
