{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}

module Main where

import Prelude

import Data.List
import Data.Set (Set)
import Data.List.Split
import qualified Data.Set as S

parseRow :: String -> Set Int
parseRow (splitOn " <-> " -> [a, splitOn ", " -> bs]) = S.fromList . map read $ a : bs
parseRow s = error s

combineGroups :: [Set Int] -> [Set Int]
combineGroups = \case
  [] -> []
  (x : ys) ->
    let (differentGroup, sameGroup) = partition (S.null . S.intersection x) ys
    in case sameGroup of
      [] -> x : combineGroups ys
      _  -> combineGroups $ foldl' S.union x sameGroup : differentGroup

main :: IO ()
main = do
  let fp = "12input.txt"
  input <- map parseRow . lines <$> readFile fp
  let combined = combineGroups input
  print $ S.size <$> find (0 `S.member`) combined
  print $ length combined
