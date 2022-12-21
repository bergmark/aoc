{-# OPTIONS -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.List.Split
import Prelude hiding (max)
import Control.Monad
import Data.Maybe
import Data.Array
import Data.Array.IO

newtype Idx = Idx Int deriving (Eq, Num, Ord, Show, Ix, Integral, Real, Enum)
newtype Val = Val { val :: Int } deriving (Eq, Num, Ord, Show, Enum)

arrayFromList :: [Val] -> IO (IOArray Idx Val)
arrayFromList list = newListArray (Idx 0, Idx $ length list - 1) list

run :: Idx -> [Int] -> Int -> IOArray Idx Val -> IO ()
run curr lens skip arr = case lens of
  [] -> pure ()
  (len : lens') -> do
    (curr', skip') <- run1 curr len skip arr
    run curr' lens' skip' arr

reverseSlice :: [Idx] -> IOArray Idx Val -> IO ()
reverseSlice ixs arr = do
  es <- getAssocs arr
  forM_ (ixs `zip` reverse ixs) $ \(from, to) ->
    writeArray arr to (fromJust $ lookup from es)
  pure ()

printArr :: IOArray Idx Val -> IO ()
printArr = print . map val <=< getElems

run1 :: Idx -> Int -> Int -> IOArray Idx Val -> IO (Idx, Int)
run1 curr len skip arr = do
  printArr arr
  print (curr, len, skip)
  reverseSlice ixs arr
  let newCurr = (curr + Idx len + Idx skip) `mod` arrLength
  pure (newCurr, skip + 1)
    where
      ixs = map ((`mod` arrLength) . (+curr)) [0..Idx len - 1]
      arrLength = 256

main :: IO ()
main = do
  let fp = "10input.txt"
  input <- readFile fp
  let lengths = read <$> splitOn "," input -- [3, 4, 1, 5]
  arr <- arrayFromList [0..255] -- [0..4]
  run 0 lengths 0 arr
  printArr arr
  a <- readArray arr (Idx 0)
  b <- readArray arr (Idx 1)
  print . val $ a * b
