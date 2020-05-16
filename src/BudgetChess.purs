module BudgetChess (runBudgetChess) where

import Imports
import Framework as F

runBudgetChess :: Effect Unit
runBudgetChess =
  F.loadSpritesheet "chess.svg" \spritesheet ->
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

-- TODO: heads-up HTML with player to move
-- TODO: slide pieces into place

budgetChess :: F.Spritesheet -> F.Game ChessState (Rank /\ File)
budgetChess spritesheet =
  { viewportSpec:
    { canvasId: "game"
    , width: 48 * 8
    , height: 48 * 8
    }
  , init: pure startingState
  , render
  , step
  }
  where

  tiles :: List (F.Sprite (Rank /\ File))
  tiles = do
    r <- enum :: _ Rank
    f <- enum :: _ File
    let r' = fromEnum r
    let f' = fromEnum f
    pure $ F.rectangle
      { fill: Just <<< F.Color $ if even (r' + f') then "beige" else "brown"
      , border: Nothing
      , x: f' * 48
      , y: r' * 48
      , width: 48
      , height: 48
      , onClick: Just (r /\ f)
      }

  spriteMap ::
    Player -> Piece ->
    { xOffset :: Int, yOffset :: Int, width :: Int, height :: Int }
  spriteMap pl pc =
    let
      xOffset = case pc of
        King -> 45 * 0
        Queen -> 45 * 1
        Bishop -> 45 * 2
        Knight -> 45 * 3
        Rook -> 45 * 4
        Pawn -> 45 * 5

      yOffset = case pl of
        White -> 0
        Black -> 45

    in
      { xOffset, yOffset, width: 45, height: 45 }

  startingState :: ChessState
  startingState =
    let
      front = replicate 8 Pawn
      back = mkList [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
      row r = zip (map (r /\ _) (FA .. FH))
      r1 = row R1 $ map (White /\ _) back
      r2 = row R2 $ map (White /\ _) front
      r7 = row R7 $ map (Black /\ _) front
      r8 = row R8 $ map (Black /\ _) back
    in
      { board: mkMap $ r1 <> r2 <> r7 <> r8
      , turn: White
      , selected: Nothing
      }

  render :: ChessState -> F.Scene (Rank /\ File)
  render { board, selected } =
    let
      pieces = board # rmMap # map \(pos@(r /\ f) /\ (pl /\ pc)) ->
        F.sprite spritesheet (spriteMap pl pc)
          { x: fromEnum f * 48 + 1
          , y: fromEnum r * 48 + 1
          , onClick: Nothing
          }

      effects = selected # foldMap \(r /\ f) ->
        pure $ F.rectangle
          { fill: Nothing
          , border: Just (3 /\ F.Color "yellow")
          , x: fromEnum f * 48
          , y: fromEnum r * 48
          , width: 48
          , height: 48
          , onClick: Nothing
          }
    in
      { sprites: tiles <> pieces <> effects }

  step :: F.Event (Rank /\ File) -> ChessState -> F.Random ChessState
  step event state@{ board, turn: pl, selected } =
    case selected, event of

      Nothing, F.Action sel ->
        case lookup sel board of

          Just (pl' /\ _) | pl' == pl ->
            pure state { selected = Just sel }

          _ ->
            pure state

      Just sel, F.Action mov | sel == mov ->
        pure state { selected = Nothing }

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

      Just sel@(r /\ f), F.KeyDown F.Key_UpArrow ->
        pure $ fromMaybe state do
          r' <- pred r
          _ /\ pc <- lookup sel board
          Just state
            { selected = Just (r' /\ f)
            , board = board
              # alter (const Nothing) sel
              # alter (const $ Just $ pl /\ pc) (r' /\ f)
            }

      Just sel@(r /\ f), F.KeyDown F.Key_DownArrow ->
        pure $ fromMaybe state do
          r' <- succ r
          _ /\ pc <- lookup sel board
          Just state
            { selected = Just (r' /\ f)
            , board = board
              # alter (const Nothing) sel
              # alter (const $ Just $ pl /\ pc) (r' /\ f)
            }

      Just sel@(r /\ f), F.KeyDown F.Key_LeftArrow ->
        pure $ fromMaybe state do
          f' <- pred f
          _ /\ pc <- lookup sel board
          Just state
            { selected = Just (r /\ f')
            , board = board
              # alter (const Nothing) sel
              # alter (const $ Just $ pl /\ pc) (r /\ f')
            }

      Just sel@(r /\ f), F.KeyDown F.Key_RightArrow ->
        pure $ fromMaybe state do
          f' <- succ f
          _ /\ pc <- lookup sel board
          Just state
            { selected = Just (r /\ f')
            , board = board
              # alter (const Nothing) sel
              # alter (const $ Just $ pl /\ pc) (r /\ f')
            }

      _, _ ->
        pure state
