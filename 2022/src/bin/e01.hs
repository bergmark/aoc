module Main where

import Data.List.Split
import Text.Read
import Data.Maybe
import Data.List
import Control.Monad

main :: IO ()
main = do
  run a "s01" 24000
  run a "e01" 69795
  run b "s01" 45000
  run b "e01" 208437

run :: (Monad m, Eq a, Show a) => (String -> m a) -> String -> a -> m ()
run f g expected = do
  actual <- f ("txt/" ++ g ++ ".txt")
  when (expected /= actual) $
    error $ concat [g, "expected: ", show expected, ", actual: ", show actual]

a :: FilePath -> IO Int
a = fmap head . sol

b :: FilePath -> IO Int
b = fmap (sum . take 3) . sol

sol :: String -> IO [Int]
sol fp
  = reverse
  . sort
  . map (sum . map (fromMaybe 0))
  . splitOn [Nothing]
  . map (readMaybe @Int)
  . lines <$> readFile fp
