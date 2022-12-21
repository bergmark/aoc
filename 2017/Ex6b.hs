{-# OPTIONS -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Monad
import Data.Array (Array)
import Data.Array.IO
import Data.List
import Data.Map (Map)
import Data.Ord
import qualified Data.Map.Strict as M

newtype Idx = Idx Int deriving (Eq, Num, Ord, Show, Ix, Integral, Real, Enum)
newtype Val = Val { val :: Int } deriving (Eq, Num, Ord, Show, Enum)

arrayFromList :: [Val] -> IO (IOArray Idx Val)
arrayFromList list = newListArray (Idx 0, Idx $ length list - 1) list

maxEntry :: IOArray Idx Val -> IO (Idx, Val)
maxEntry arr =
  maximumBy (comparing comp) <$> getAssocs arr
  where
    comp (a, b) = (b, negate a)

run1 :: IOArray Idx Val -> (Idx, Idx) -> IO ()
run1 arr (_, end) = do
  (maxIdx, maxVal) <- maxEntry arr
  writeArray arr maxIdx 0
  let indices = map ((`mod` (end + 1)) . (+ maxIdx) . Idx . val) [1..maxVal]
  forM_ indices $ \i ->
    writeArray arr i . (+1) =<< readArray arr i

run :: Int -> Map Int (Array Idx Val) -> IOArray Idx Val -> (Idx, Idx) -> IO (Int, Maybe Int)
run cycles cache arr bounds = do
  frozen <- freeze arr
  if frozen `elem` cache
  then pure (cycles, fmap ((cycles -) . fst) . find ((== frozen) . snd) $ M.toList cache)
  else do
    run1 arr bounds
    run (cycles + 1) (M.insert cycles frozen cache) arr bounds

main :: IO ()
main = do
  let list = [2,8,8,5,4,2,3,1,5,5,1,2,15,13,5,14]
  arr <- arrayFromList list
  print =<< run 0 mempty arr =<< getBounds arr
