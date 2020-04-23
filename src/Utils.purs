module Phutball.Utils where

import Prelude
import Data.Char (fromCharCode)
import Data.Foldable (class Foldable, foldr)
import Data.Maybe (Maybe)
import Data.String.CodeUnits (singleton)

foldrM :: forall a b f m. Monad m => Foldable f => (a -> b -> m b) -> b -> f a -> m b
foldrM f = foldr (flip (>>=) <<< f) <<< pure

toLetter :: Int -> Maybe String
toLetter = map singleton <<< fromCharCode <<< (+) 65
