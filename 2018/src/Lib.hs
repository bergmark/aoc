module Lib
  ( module Prelude
  , module Lib
  , module Data.Maybe
  , module Data.List
  , module Control.Monad
  , module Data.String.Conversions
  , module Data.String.Conversions.Monomorphic
  ) where

import RIO
import Data.Maybe (fromJust)
import Data.List (cycle, head, scanl', groupBy, sort, find)
import Prelude (print, putStrLn)
import Data.String.Conversions (cs)
import Data.String.Conversions.Monomorphic
import Control.Monad
import System.IO.Unsafe
import Data.Text qualified as T
import Data.Text.IO qualified as T

readLines :: (Text -> Maybe a) -> FilePath -> IO [a]
readLines parse = pure . map (\(i, l) -> fromMaybe (todo (i,l)) . parse $ l) . zip [(1::Int)..] . T.lines <=< T.readFile

todo :: (HasCallStack, Show a) => a -> b
todo = error . show

read :: Read a => Text -> a
read s = fromMaybe (todo ("read", s)) . readMaybe . cs $ s

infixr 1 <$<
(<$<) :: Monad m => (b -> c) -> (a -> m b) -> a -> m c
(<$<) p = (pure . p <=<)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b
trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

assertEq :: (Show a, Eq a) => a -> a -> IO String
assertEq x y
 | x == y = pure "OK"
 | otherwise = error $ show x <> " /= " <> show y

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left a) = Just a
leftToMaybe (Right _) = Nothing

dbg :: Show a => a -> a
dbg v = unsafePerformIO $ print v >> pure v

infix 0 `toMaybe`
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

infixr 8 .:
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)

-- f g h x y = g $ h x y
