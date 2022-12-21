{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Tree
import Data.List.Split
import Data.List

newtype Name = Name { unName :: String } deriving (Eq, Show)
newtype Weight = Weight { unWeight :: Int } deriving (Eq, Show, Ord, Num)
newtype TotalWeight = TotalWeight { unTotalWeight :: Int } deriving (Eq, Show, Ord, Num)

parseLine :: String -> (Name, Weight, [Name])
parseLine s = (name, weight, ns)
  where
    comps :: [String] = splitOn " " s
    (name, weight, ns) =
      case comps of
        [n, w] ->
          ( Name n
          , toWeight w
          , []
          )
        n : w : "->" : cs ->
          ( Name n
          , toWeight w
          , map (Name . takeWhile (/= ',')) cs
          )
        l -> error $ "Bad line: " ++ show l
    toWeight = Weight . read . tail . reverse . tail . reverse

names :: [(Name, a, b)] -> [Name]
names = map $ \(n, _, _) -> n

children :: [(Name, a, [Name])] -> [Name]
children = concatMap $ \(_, _, cs) -> cs

buildTree :: [(Name, Weight, [Name])] -> Name -> Tree (Name, Weight)
buildTree nodes n = case find (\(n', _, _) -> n' == n) nodes of
  Nothing -> error $ "Couldn't find name: " ++ unName n
  Just (_, w, cs) ->
    Node (n, w) (map (buildTree nodes) cs)

trd :: (a, b, c) -> c
trd (_, _, c) = c

totalWeights :: Tree (Name, Weight) -> Tree (Name, Weight, TotalWeight)
totalWeights (Node (name, w) sub) =
  Node (name, w, TotalWeight (unWeight w) + sum (map (trd . rootLabel) cs)) cs
  where
    cs = map totalWeights sub

oddOneOut :: Eq a => [a] -> Maybe (a, a)
oddOneOut = \case
  [] -> Nothing
  [_] -> Nothing
  [a, b] -> if a == b then Nothing else Just (a, b)
  [a, b, c]
    | a == b && b == c -> Nothing
    | a == b && b /= c -> Just (c, a)
    | a == c && b /= c -> Just (b, a)
    | b == c && a /= b -> Just (a, b)
    | otherwise -> error "Impossible"
  a : b : c : xs
    | a == b -> (, a) <$> find (/= a) (c : xs)
    | otherwise -> oddOneOut [a, b, c]

findBadWeight :: Tree (Name, Weight, TotalWeight) -> Maybe (TotalWeight, TotalWeight)
findBadWeight n = oddOneOut . map (trd . rootLabel) $ subForest n

showNode :: (Name, Weight) -> String
showNode (n, w) = unName n ++ "(" ++ show (unWeight w) ++ ")"

showNode' :: (Name, Weight, TotalWeight) -> String
showNode' (n, w, w') = unName n ++ "(" ++ show (unWeight w) ++ ")[" ++ show (unTotalWeight w') ++ "]"

main :: IO ()
main = do
  f <- map parseLine . lines <$> readFile "Ex7input.txt"
  let ns = names f
  let ch = children f
  let [root] = ns \\ ch
  print root
  let tree = buildTree f root
  putStrLn . drawTree $ fmap showNode tree
  let tree' = totalWeights tree
  putStrLn . drawTree $ fmap showNode' tree'
  let bad = findBadWeight tree'
  print bad
