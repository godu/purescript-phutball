module Test.Phutball.Domain where

import Prelude
import Data.Map (insert, lookup, member, size)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Phutball.Domain (Action(..), Piece(..), Position, Board, applyAction, defaultBoard, foldrM)
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = do
  it "defaultBoard"
    let
      middlePosition = Tuple 10 7
    in
      do
        defaultBoard `shouldSatisfy` ((==) 1 <<< size)
        defaultBoard `shouldSatisfy` member middlePosition
        defaultBoard `shouldSatisfy` ((==) (pure Ball) <<< lookup middlePosition)
  it "applyAction"
    let
      position :: Position
      position = Tuple 9 7

      action :: Action
      action = Add position

      expected :: Maybe Board
      expected = pure $ insert position Man defaultBoard
    in
      do
        action `applyAction` defaultBoard `shouldEqual` expected
        foldrM applyAction defaultBoard [ action ] `shouldEqual` expected
