{-# LANGUAGE StrictData #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import Prelude hiding (Either (..), max)

-- import Data.List
import Data.Map (Map)
-- import Data.Ord (comparing)
import Data.Semigroup
-- import Debug.Trace
import qualified Data.Map.Strict as M

ex3input :: Int
ex3input = 265149

data Point = Point { px :: X, py :: Y } deriving (Eq, Ord)
instance Show Point where
  show p = "(" <> show (unX (px p)) <> "," <> show (unY (py p)) <> ")"

newtype X = X { unX :: Int } deriving (Eq, Num, Ord, Show)
newtype Y = Y { unY :: Int } deriving (Eq, Num, Ord, Show)

data Direction = Up | Down | Right | Left deriving (Eq, Show)

-- showPop :: Int -> [(Point, Int)]
-- showPop = sortBy (comparing snd) . M.toList . popAux

main :: IO ()
main = do
  let pos = popAux ex3input
  print pos
  print $ unX (px pos) + unY (py pos)

popAux :: Int -> Point
popAux max =
  populate (Point 0 0) (M.fromList [(Point 0 0, 1)]) Down 1 (Max max)

populate :: Point -> Map Point Int -> Direction -> Int -> Max Int -> Point
populate lastPoint@(Point lastX lastY) vs lastDir lastNum max@(Max maxNum) =
  case lastNum >= maxNum of
    True -> lastPoint
    False ->
      case lastDir of
        Right ->
          case M.lookup (Point lastX (lastY - 1)) vs of
            Nothing -> goUp
            Just _  -> goRight
        Up ->
          case M.lookup (Point (lastX - 1) lastY) vs of
            Nothing -> goLeft
            Just _  -> goUp
        Left ->
          case M.lookup (Point lastX (lastY + 1)) vs of
            Nothing -> goDown
            Just _  -> goLeft
        Down ->
          case M.lookup (Point (lastX + 1) lastY) vs of
            Nothing -> goRight
            Just _  -> goDown
  where
    nextNum    = lastNum + 1
    goLeft     = trace "goLeft"  $ pop (Point (lastX - 1) lastY) Left
    goRight    = trace "goRight" $ pop (Point (lastX + 1) lastY) Right
    goUp       = trace "goUp"    $ pop (Point lastX (lastY - 1)) Up
    goDown     = trace "goDown"  $ pop (Point lastX (lastY + 1)) Down
    pop xy dir = case M.lookup xy vs of
      Nothing -> populate xy (M.insert xy nextNum vs) dir nextNum max
      Just v -> error $ "Tried to go " <> show dir <> " and insert: " <> show nextNum <> " into: " <> show xy <> ", but it already contained: " <> show v

trace :: String -> a -> a
trace _ = id
