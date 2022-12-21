{-# OPTIONS -Wall #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad
import Data.Ord
import Data.List
import Data.Array.IO
import Data.Set (Set)
import Data.Array (Array, elems)
import qualified Data.Set as S

newtype Idx = Idx { idx :: Int } deriving (Eq, Num, Ord, Show, Ix, Integral, Real, Enum)
newtype Val = Val { val :: Int } deriving (Eq, Num, Ord, Show, Enum)

memory :: [Val] -> IO (IOArray Idx Val)
memory list = newListArray (Idx 0, Idx $ length list - 1) list

comp :: Num a => (a, b) -> (b, a)
comp (a, b) = (b, negate a)

maxVal :: IOArray Idx Val -> IO (Idx, Val)
maxVal arr =
  maximumBy (comparing comp) <$> getAssocs arr

run1 :: IOArray Idx Val -> IO ()
run1 arr = do
  (begin, end) <- getBounds arr
  (maxIdx, maxVal) <- maxVal arr
  writeArray arr maxIdx 0
  let indices = map ((`mod` (end + 1)) . (+ maxIdx) . Idx . val) [1..maxVal]
  putStrLn $ "indices: " ++ show (map idx indices)
  forM_ indices $ \i -> do
    oldVal <- readArray arr i
    writeArray arr i (oldVal + 1)
  pure ()

run :: Int -> Set (Array Idx Val) -> IOArray Idx Val -> IO Int
run cycles cache arr = do
  frozen <- freeze arr
  putStrLn $ "run: " ++ show (map val $ elems frozen)
  if frozen `elem` cache
  then pure cycles
  else do
    run1 arr
    newFrozen <- freeze arr
    unless (sum (elems frozen) == sum (elems newFrozen)) $
      error $ show (sum (elems frozen)) ++ "/=" ++ show (sum (elems newFrozen))
    run (cycles + 1) (S.insert frozen cache) arr

main :: IO ()
main = do
  let list = [2,8,8,5,4,2,3,1,5,5,1,2,15,13,5,14] :: [Val]
  -- let list = [0,2,7,0]
  mem <- memory list
  print =<< run 0 mempty mem
  print =<< getElems mem
