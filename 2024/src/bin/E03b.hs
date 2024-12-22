module Main where

import Lib

s :: String
s = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

main :: IO ()
main = print . (== 48) . sol $ s

sol :: String -> Int
sol = snd . foldl' f (True, 0) . tails

f :: (Bool, Int) -> String -> (Bool, Int)
f (b, p) s
  | "do()" `isPrefixOf` s = (True, p)
  | "don't()" `isPrefixOf` s = (False, p)
  | not b = (b, p)
  | otherwise = maybe (b, p) ((b,) . (p+) . uncurry (*)) . parseMul $ s

parseMul :: String -> Maybe (Int, Int)
parseMul s = do
  (x, s) <- str "mul(" s >>= num
  (y, s) <- str "," s >>= num
  str ")" s >> pure (x, y)

str :: String -> String -> Maybe String
str x s = guard (x `isPrefixOf` s) >> pure (drop (length x) s)

num :: String -> Maybe (Int, String)
num s = do
  let (digs, s') = span isDigit s
  num <- readMaybe digs
  pure (num, s')
