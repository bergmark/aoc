{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Function
import Debug.Trace

main :: IO ()
main = do
  c :: [Int] <- map read . lines <$> readFile "input.txt"
  print . sum . map f $ c
  where
    f :: Int -> Int
    f = (\x -> x - 2) . (`div` 3)

-- 3255932

main2 :: IO ()
main2 = do
  print . sum . map g . map read . lines =<< readFile "input.txt"
  where
    g :: Int -> Int
    g i = if s == 0 then 0 else (s + g s)
      where
        s = max 0 . subtract 2 . (`div` 3) $ i

-- 4881041

main2b :: IO ()
main2b = do
  print . sum . map (fix (\f i -> let s = max 0 . subtract 2 . (`div` 3) $ i in if s == 0 then 0 else s + f s) . read) . lines =<< readFile "input.txt"
