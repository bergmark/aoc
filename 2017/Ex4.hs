{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.List.Split

isAnagram (x, y) = sort x == sort y

main :: IO ()
main = do
  ws <- length . filter (\ws -> all (not . isAnagram) [(w1, w2) | w1 <- ws, w2 <- ws, w1 /= w2]) . filter (\v -> length v == length (nub v)) . map words . lines <$> readFile "Ex4.txt"
  print ws
