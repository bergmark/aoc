{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List.Split

main :: IO ()
main =
  print =<< s2b1 <$> readFile "Ex2.txt"

parse :: (Num a, Read a) => String -> [[a]]
parse = map (map read . splitOn "\t") . lines

s2a1 :: String -> Integer
s2a1 = sum . map (\v -> maximum v - minimum v) . parse

s2b1 = sum . map (\v -> head [x `div` y | x <- v, y <- v, x /= y, x `mod` y == 0]) . parse
