{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (max)

import Data.List
import Data.List.Split

newtype X = X { unX :: Int } deriving (Eq, Num, Show, Integral, Real, Enum, Ord)
newtype Y = Y { unY :: Int } deriving (Eq, Num, Show, Integral, Real, Enum, Ord)

data Point = Point { x :: X, y :: Y } deriving (Eq, Show)

add :: Point -> Point -> Point
add a b = Point (x a + x b) (y a + y b)

sub :: Point -> Point -> Point
sub a b = Point (x a - x b) (y a - y b)

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

distanceToOrigo :: Point -> Int
distanceToOrigo Point{..} = abs (unX x) + abs (unY y)

shortestPath1 :: Point -> (Direction, Point)
shortestPath1 = undefined

straightTowardsOrigo :: Point -> (Direction, Point)
straightTowardsOrigo p@Point{..}
  | y > 0 = (N, p `add` offset N)
  | y < 0 = (S, p `add` offset S)
  | otherwise = error $ "straight: Already at origo? " ++ show p

diagonallyTowardsOrigo :: Point -> (Direction, Point)
diagonallyTowardsOrigo p@Point{..}
  | x == 0 = error $ "diagonally: x was 0, " ++ show p
  | x > 0 && y >= 0 = (NW, p `add` offset NW)
  | x < 0 && y <= 0 = (SE, p `add` offset SE)
  | x > 0 && y <= 0 = (SW, p `add` offset SW)
  | x < 0 && y >= 0 = (NE, p `add` offset NE)
  | otherwise = error $ "diagonally: Too close to origo? " ++ show p

shortestPathToOrigo :: Point -> [Direction] -> IO [Direction]
shortestPathToOrigo p@Point{..} ds
  | isOrigo p = pure ds
  | otherwise = do
    print p
    let (d', p') =
          if x == 0
          then straightTowardsOrigo p
          else diagonallyTowardsOrigo p
    putStrLn $ show p ++ " -> " ++ show d' ++ ", " ++ show p'
    if distanceToOrigo p' >= distanceToOrigo p
    then error $ "Distance did not decrease going from " ++ show p ++ " in the direction " ++ show d' ++ " ending up at " ++ show p'
    else shortestPathToOrigo p' (d' : ds)

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
  let pos = foldl' (\p d -> p `add` offset d) (Point 0 0) input
  putStrLn $ "pos: " ++ show pos
  sp <- shortestPathToOrigo pos []
  print sp
  print $ length sp
  pure ()
