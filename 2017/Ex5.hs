{-# OPTIONS -Wall #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ex5 where

import Data.Array.IO

memory :: MArray a Int m => [Int] -> m (a Int Int)
memory list = newListArray (0, length list - 1) list

newtype PC = PC { unPC :: Int } deriving (Eq, Num, Ord, Show)
newtype Count = Count { unCount :: Int } deriving (Eq, Num, Ord, Show)

run :: PC -> Count -> IOArray Int Int -> IO (PC, Count)
run pc count arr = do
  -- print . (pc, ) =<< getElems arr
  (begin, end) <- getBounds arr
  if unPC pc < begin || unPC pc > end
  then pure (pc, count)
  else do
    offset <- readArray arr (unPC pc)
    writeArray arr (unPC pc) (offset + 1)
    run (pc + PC offset) (count + 1) arr

main :: IO ()
main = do
  list <- map read . lines <$> readFile "Ex5.txt"
  mem <- memory list
  print =<< getElems mem
  print =<< run 0 0 mem
