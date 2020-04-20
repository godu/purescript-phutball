module Phutball.Domain where

import Prelude
import Data.Foldable (class Foldable, foldr)
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

top :: Int
top = 20

bottom :: Int
bottom = 0

left :: Int
left = 0

right :: Int
right = 14

isBundedPosition :: Position -> Boolean
isBundedPosition (Tuple row column) = row > bottom && row < top && column >= left && column <= right

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
  rowMiddle = (top + bottom) / 2

  columnMiddle :: Int
  columnMiddle = (left + right) / 2

data Action
  = Add Position
  | Pass (List Position)

type Game
  = List Action

applyAction :: Action -> Board -> Maybe Board
applyAction (Add position) board
  | not $ member position board && isBundedPosition position = pure $ insert position Man board

applyAction board _ = Nothing

foldrM :: forall a b f m. Monad m => Foldable f => (a -> b -> m b) -> b -> f a -> m b
foldrM f = foldr (flip (>>=) <<< f) <<< pure
