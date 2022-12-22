module Main where

import RIO

import Data.Text qualified as T
import Data.IntSet qualified as H
import Lib

main :: IO ()
main = do
  sol "A"   a "txt/e01.txt"    525
  sol "Bsa" b "txt/s01a.txt"     0
  sol "Bsb" b "txt/s01b.txt"    10
  sol "Bsc" b "txt/s01c.txt"     5
  sol "Bsd" b "txt/s01d.txt"    14
  sol "B"   b "txt/e01.txt"  75749
  where
    sol ex s f res = print . (ex,) =<< assertEq res . s =<< readLines parse f
    parse :: Text -> Maybe Int
    parse = pure . uncurry ($) . (sign *** read) <=< T.uncons
    sign = \case
      '+' -> id
      '-' -> negate
      v -> todo ("sign", v)
    a :: [Int] -> Int
    a = sum
    b :: [Int] -> Int
    b = head . lefts . scanl' d (Right (0, H.singleton 0, 0)) . cycle
    d :: Accum -> Int -> Accum
    d (Left v) _ = Left v
    d (Right (oldFreq, h, w)) change =
      if newFreq `H.member` h then Left newFreq else Right (newFreq, h', w + 1)
      where
        newFreq = oldFreq + change
        h' = H.insert newFreq h

type Accum = Either Int (Int, IntSet, Int)
