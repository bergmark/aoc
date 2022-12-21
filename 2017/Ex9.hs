{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Data.Data
import Data.Generics.Uniplate.Data
import System.IO
import Text.Parsec

newtype Garbage = Garbage [Content]
  deriving (Data, Eq, Show)

data Elem a
  = Gr (Group a)
  | Ga Garbage
  deriving (Data, Eq, Show)

data Group a = Group a [Elem a]
  deriving (Data, Eq, Show)

data Content
  = Escaped Char
  | NotEscaped Char
  deriving (Data, Eq, Show)

parseEscape :: Stream s m Char => ParsecT s u m Content
parseEscape
  = char '!'
  *> fmap Escaped anyChar

parseChar :: Stream s m Char => ParsecT s u m Content
parseChar =
  NotEscaped <$> noneOf "!>"

parseGarbage :: Stream s m Char => ParsecT s u m Garbage
parseGarbage
   = char '<'
  *> fmap Garbage (many $ parseEscape <|> parseChar)
  <* char '>'

parseElem :: Stream s m Char => ParsecT s u m (Elem ())
parseElem = fmap Gr parseGroup <|> fmap Ga parseGarbage

parseGroup :: Stream s m Char => ParsecT s u m (Group ())
parseGroup
  = char '{'
  *> fmap (Group ()) (parseElem `sepBy` char ',')
  <* char '}'

points :: Int -> Group () -> Group Int
points i (Group () elems) = Group i . map (elemPoints i) $ elems

elemPoints :: Int -> Elem () -> Elem Int
elemPoints i = \case
  Gr gr -> Gr $ points (i+1) gr
  Ga ga -> Ga ga

sumPoints :: Group Int -> Int
sumPoints = sum . universeBi

countGarbage :: Group () -> Int
countGarbage = sum . map countContent . universeBi
  where
    countContent :: Content -> Int
    countContent = \case
      Escaped    _ -> 0
      NotEscaped _ -> 1

main :: IO ()
main = do
  let fp = "9input.txt"
  input <- readFile fp
  let parsed = parse parseGroup fp input
  case parsed of
    Left e -> hPrint stderr e
    Right r -> do
      let ps = points 1 r
      print . sumPoints $ ps
      print . countGarbage $ r
