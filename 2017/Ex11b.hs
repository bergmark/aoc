{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Prelude

import Data.Ord
import Data.List
import Data.List.Split

newtype X = X Int deriving (Eq, Num, Show, Integral, Real, Enum, Ord)
newtype Y = Y Int deriving (Eq, Num, Show, Integral, Real, Enum, Ord)

data Point = Point { x :: X, y :: Y } deriving (Eq, Show)

add :: Point -> Point -> Point
add a b = Point (x a + x b) (y a + y b)

isOrigo :: Point -> Bool
isOrigo Point{..} = x == 0 && y == 0

data Direction = N | NE | SE | S | SW | NW deriving (Eq, Show)

offset :: Direction -> Point
offset = \case
  N  -> f ( 0, -2)
  S  -> f ( 0,  2)
  NE -> f ( 2, -1)
  SE -> f ( 2,  1)
  NW -> f (-2, -1)
  SW -> f (-2,  1)
  where
    f = uncurry Point

go :: Direction -> Point -> (Direction, Point)
go dir p = (dir, p `add` offset dir)

straightTowardsOrigo :: Point -> (Direction, Point)
straightTowardsOrigo p@Point{..}
  | y > 0 = go N p
  | y < 0 = go S p
  | otherwise = error $ "straight: Already at origo? " ++ show p

diagonallyTowardsOrigo :: Point -> (Direction, Point)
diagonallyTowardsOrigo p@Point{..}
  | x == 0 = error $ "diagonally: x was 0, " ++ show p
  | x > 0 && y >= 0 = go NW p
  | x < 0 && y <= 0 = go SE p
  | x > 0 && y <= 0 = go SW p
  | x < 0 && y >= 0 = go NE p
  | otherwise = error $ "diagonally: Too close to origo? " ++ show p

shortestPathToOrigo :: Point -> [Direction] -> [Direction]
shortestPathToOrigo p@Point{..} ds
  | isOrigo p = ds
  | otherwise =
    let (d', p') = if x == 0 then straightTowardsOrigo p else diagonallyTowardsOrigo p
    in shortestPathToOrigo p' (d' : ds)

parseDir :: String -> Direction
parseDir = \case
  "s" -> S
  "n" -> N
  "sw" -> SW
  "nw" -> NW
  "ne" -> NE
  "se" -> SE
  s -> error $ "parseDir: " ++ show s

main :: IO ()
main = do
  let fp = "11input.txt"
  input <- map parseDir . splitOn "," . head . lines <$> readFile fp
  print input
  -- Note that paths are inverted!!
  -- let input = [NE, NE, NE] -- [SW, SW, SW]
  -- let input = [NE, NE, SW, SW] -- => []
  -- let input = [NE, NE, S, S] -- => [NW, NW]
  -- let input = [SE, SW, SE, SW, SW] -- => [S, S, W]
  let poss = scanl' (\p d -> p `add` offset d) (Point 0 0) input
  let sps = maximumBy (comparing length) . map (`shortestPathToOrigo` []) $ poss
  print $ length sps
