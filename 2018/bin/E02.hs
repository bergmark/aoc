module Main where

import RIO

import Lib

main :: IO ()
main = do
  sol "As" a "txt/s02.txt"  12
  sol "A"  a "txt/e02.txt"  6642
  sol "Bs" b "txt/s02b.txt" "fgij"
  sol "B"  b "txt/e02.txt"  "cvqlbidheyujgtrswxmckqnap"
  where
    sol ex s f res = print . (ex,) =<< assertEq res . s =<< readLines parse f
    parse :: Text -> Maybe Text
    parse = pure
a :: [Text] -> Int
a =  uncurry (*) . (sum . map fst &&& sum . map snd) . map (a2 . takeWhile (< 4) . dropWhile (< 2) . sort . map length . groupBy (==) . sort . toString)
  where
    a2 v = (if 2 `elem` v then 1 else 0, if 3 `elem` v then 1 else 0)

b :: [Text] -> String
b = head . b2 . map toString
  where
    b2 :: [String] -> [String]
    b2 ts = concatMap (flip mapMaybe ts . commonCharsExcept1) ts
commonCharsExcept1 :: String -> String -> Maybe String
commonCharsExcept1 = uncurry toMaybe . ((== 1) . length . lefts &&& rights) .: diff

diff :: String -> String -> [Either (Char, Char) Char]
diff = zipWith (\x y -> if x == y then Right x else Left (x, y))
