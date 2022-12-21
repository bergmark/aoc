{-# OPTIONS -Wall #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import Prelude hiding (Either (..), max)

import Data.Maybe
import Data.Map (Map)
import Data.Semigroup
import qualified Data.Map.Strict as M

ex3input :: Int
ex3input = 265149

data Point = Point { px :: X, py :: Y } deriving (Eq, Ord)
instance Show Point where
  show p = "(" <> show (unX (px p)) <> "," <> show (unY (py p)) <> ")"

newtype X = X { unX :: Int } deriving (Eq, Num, Ord, Show)
newtype Y = Y { unY :: Int } deriving (Eq, Num, Ord, Show)

data Direction = Up | Down | Right | Left deriving (Eq, Show)

main :: IO ()
main =
  print $ populate (Point 0 0) (M.fromList [(Point 0 0, 1)]) Down 1 (Max ex3input)

populate :: Point -> Map Point Int -> Direction -> Int -> Max Int -> (Point, Int)
populate lastPoint@(Point lastX lastY) vs lastDir lastNum max@(Max maxNum) =
  case lastNum > maxNum of
    True -> (lastPoint, lastNum)
    False ->
      case lastDir of
        Right -> go goUp    goRight $ Point lastX       (lastY - 1)
        Up    -> go goLeft  goUp    $ Point (lastX - 1) lastY
        Left  -> go goDown  goLeft  $ Point lastX       (lastY + 1)
        Down  -> go goRight goDown  $ Point (lastX + 1) lastY
  where
    go empty filled p =
      case M.lookup p vs of
        Nothing -> empty
        Just _  -> filled
    nextNum (Point x y)
      = look (Point x (y - 1))
      + look (Point x (y + 1))
      + look (Point (x - 1) y)
      + look (Point (x + 1) y)
      + look (Point (x + 1) (y + 1))
      + look (Point (x - 1) (y - 1))
      + look (Point (x + 1) (y - 1))
      + look (Point (x - 1) (y + 1))
    look p = fromMaybe 0 $ M.lookup p vs
    goLeft     = pop (Point (lastX - 1) lastY) Left
    goRight    = pop (Point (lastX + 1) lastY) Right
    goUp       = pop (Point lastX (lastY - 1)) Up
    goDown     = pop (Point lastX (lastY + 1)) Down
    pop xy dir = case M.lookup xy vs of
      Nothing -> populate xy (M.insert xy theNextNum vs) dir theNextNum max
      Just v -> error $ "Tried to go " <> show dir <> " and insert: " <> show theNextNum <> " into: " <> show xy <> ", but it already contained: " <> show v
      where
        theNextNum = nextNum xy
