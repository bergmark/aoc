{-# OPTIONS -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude

import Data.Char
import Numeric
import Data.Bits
import Data.List
import Control.Monad
import Data.Maybe
import Data.Array
import Data.Array.IO

newtype Idx = Idx Int deriving (Eq, Num, Ord, Show, Ix, Integral, Real, Enum)
newtype Val = Val { val :: Int } deriving (Eq, Num, Ord, Show, Enum, Bits, Integral, Real)

arrayFromList :: [Val] -> IO (IOArray Idx Val)
arrayFromList list = newListArray (Idx 0, Idx $ length list - 1) list

reverseSlice :: [Idx] -> IOArray Idx Val -> IO ()
reverseSlice ixs arr = do
  es <- getAssocs arr
  forM_ (ixs `zip` reverse ixs) $ \(from, to) ->
    writeArray arr to (fromJust $ lookup from es)

printArr :: IOArray Idx Val -> IO ()
printArr = print . map val <=< getElems

run1 :: Idx -> Int -> Int -> IOArray Idx Val -> IO (Idx, Int)
run1 curr len skip arr = do
  reverseSlice ixs arr
  pure
    ( (curr + Idx len + Idx skip) `mod` arrLength
    , skip + 1
    )
    where
      ixs = map ((`mod` arrLength) . (+curr)) [0..Idx len - 1]
      arrLength = 256

run :: Idx -> [Int] -> Int -> IOArray Idx Val -> IO (Idx, Int)
run curr lens skip arr = case lens of
  [] -> pure (curr, skip)
  (len : lens') -> do
    (curr', skip') <- run1 curr len skip arr
    run curr' lens' skip' arr

run64 :: Word -> Idx -> [Int] -> Int -> IOArray Idx Val -> IO ()
run64 n curr lens skip arr =
  if n == 0
  then pure ()
  else do
    (curr', skip') <- run curr lens skip arr
    run64 (n - 1) curr' lens skip' arr

mkDenseHash :: [Val] -> [Val]
mkDenseHash = \case
  [] -> []
  xs -> foldl1' xor (take 16 xs) : mkDenseHash (drop 16 xs)

toHex :: [Val] -> [String]
toHex = map (pad . flip showHex "")
  where
    pad [c] = ['0', c]
    pad s   = s

main :: IO ()
main = do
  let fp = "10input.txt"
  input <- head . lines <$> readFile fp
  let lens1 = map ord input
  let lens2 = [17, 31, 73, 47, 23]
  let lens = lens1 ++ lens2
  arr <- arrayFromList [0..255]
  run64 64 0 lens 0 arr
  sparseHash <- getElems arr
  let denseHash = mkDenseHash sparseHash
  print . length $ denseHash
  print . map val $ denseHash
  print . toHex $ denseHash
  print . concat $ toHex denseHash
  pure ()
