module BudgetChess (runBudgetChess) where

import Imports
import Framework as F

runBudgetChess :: Effect Unit
runBudgetChess =
  F.loadSpritesheet "Chess_Pieces_Sprite.svg" \spritesheet ->
    F.runGame $ budgetChess spritesheet

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
data File = FA | FB | FC | FD | FE | FF | FG | FH
data Player = White | Black
data Piece = Pawn | Rook | Knight | Bishop | Queen | King

derive instance eqRank :: Eq Rank
derive instance eqFile :: Eq File
derive instance eqPlayer :: Eq Player
derive instance ordRank :: Ord Rank
derive instance ordFile :: Ord File
derive instance genericRank :: Generic Rank _
derive instance genericFile :: Generic File _

instance enumRank ∷ Enum Rank where
  succ = genericSucc
  pred = genericPred

instance boundedRank ∷ Bounded Rank where
  top = genericTop
  bottom = genericBottom

instance boundedEnumRank ∷ BoundedEnum Rank where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance enumFile ∷ Enum File where
  succ = genericSucc
  pred = genericPred

instance boundedFile ∷ Bounded File where
  top = genericTop
  bottom = genericBottom

instance boundedEnumFile ∷ BoundedEnum File where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

nextPlayer :: Player -> Player
nextPlayer White = Black
nextPlayer Black = White

type ChessState =
  { board :: Map (Rank /\ File) (Player /\ Piece)
  , turn :: Player
  , selected :: Maybe (Rank /\ File)
  }

budgetChess :: F.Spritesheet -> F.Game ChessState (Rank /\ File)
budgetChess spritesheet =
  { viewportSpec:
    { canvasId: "game"
    , width: 1280
    , height: 800
    }
  , init: pure startingState
  , render
  , step
  }
  where

  startingState :: ChessState
  startingState =
    { board: mkMap $ r1 <> r2 <> r7 <> r8
    , turn: White
    , selected: Nothing
    }
    where

    front = replicate 8 Pawn
    back = mkList [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    row r = zip (map (r /\ _) (FA .. FH))
    r1 = row R1 $ map (White /\ _) back
    r2 = row R2 $ map (White /\ _) front
    r7 = row R7 $ map (Black /\ _) front
    r8 = row R8 $ map (Black /\ _) back

  render :: ChessState -> F.Scene (Rank /\ File)
  render = undefined

  step :: F.Event (Rank /\ File) -> ChessState -> F.Random ChessState
  step event state@{ board, turn: pl, selected } =
    case selected, event of

      Nothing, F.Action pos ->
        case lookup pos board of

          Just (pl' /\ _) | pl' == pl ->
            pure state { selected = Just pos }

          _ ->
            pure state

      Just sel, F.Action mov ->
        case lookup sel board, lookup mov board of

          _, Just (pl' /\ _) | pl == pl' ->
            pure state

          Just (_ /\ pc), _ ->
            pure state
              { selected = Nothing
              , turn = nextPlayer pl
              , board = board
                # alter (const Nothing) sel
                # alter (const $ Just (pl /\ pc)) mov
              }

          _, _ ->
            pure state
