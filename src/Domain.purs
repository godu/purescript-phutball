module Phutball.Domain where

import Prelude
import Data.List.Lazy (List)
import Data.Map (Map, member, insert, singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data Player
  = Ohs
  | Eks

derive instance eqPlayer :: Eq Player

instance showPlayer :: Show Player where
  show Ohs = "Ohs"
  show Eks = "Eks"

type Position
  = Tuple Int Int

top_ :: Int
top_ = 20

bottom_ :: Int
bottom_ = 0

left_ :: Int
left_ = 0

right_ :: Int
right_ = 14

isBundedPosition :: Position -> Boolean
isBundedPosition (Tuple row column) = row > bottom_ && row < top_ && column >= left_ && column <= right_

data Piece
  = Ball
  | Man

derive instance eqPiece :: Eq Piece

instance showPiece :: Show Piece where
  show Ball = "Ball"
  show Man = "Man"

type Board
  = Map Position Piece

defaultBoard :: Board
defaultBoard = singleton middlePosition Ball
  where
  middlePosition :: Position
  middlePosition = Tuple rowMiddle columnMiddle

  rowMiddle :: Int
  rowMiddle = (top_ + bottom_) / 2

  columnMiddle :: Int
  columnMiddle = (left_ + right_) / 2

data Action
  = Add Position
  | Pass (List Position)

type Game
  = List Action

applyAction :: Action -> Board -> Maybe Board
applyAction (Add position) board
  | not $ member position board && isBundedPosition position = pure $ insert position Man board

applyAction board _ = Nothing
