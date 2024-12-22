module Lib
  ( module Debug.Trace
  , module Control.Monad
  , module Data.Char
  , module Text.Read
  , module Data.Maybe
  , module Data.Function
  , module Data.List
  , module Lib
  ) where

import Debug.Trace
import Control.Monad
import Data.Char
import Text.Read
import Data.Maybe
import Data.Function
import Data.List

tr :: (Show a, Show b) => a -> b -> b
tr a b = traceShow (a, b) b
