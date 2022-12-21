{-# OPTIONS -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (max)
import Data.Semigroup
import Data.List
import Control.Monad
import Data.List.Split
import Data.Map (Map)
import Data.Maybe
import Safe
import qualified Data.Map.Strict as M

newtype Reg = Reg { unReg :: String } deriving (Eq, Ord)
data Op = Inc | Dec
newtype CondOp = CondOp { unCondOp :: Val -> Val -> Bool }
newtype Val = Val { unVal :: Int } deriving (Eq, Num, Ord)

data Instruction = Instruction
  { to      :: Reg
  , op      :: Op
  , val     :: Val
  , from    :: Reg
  , condOp  :: CondOp
  , condVal :: Val
  }

data Err
  = BadOp      String [String]
  | BadVal     String [String]
  | BadCondOp  String [String]
  | BadIf      String [String]
  | BadCondVal String [String]
  | BadLength         [String]
  deriving (Eq, Show)

parser :: String -> Either Err Instruction
parser (splitOn " " -> s) =
  case s of
    [Reg -> to, opS, valS, ifS, Reg -> from, condOpS, condValS] -> do
      op <- case opS of
        "inc" -> Right Inc
        "dec" -> Right Dec
        _     -> Left $ BadOp opS s
      val <- case readMay valS of
        Nothing -> Left $ BadVal valS s
        Just v -> Right $ Val v
      unless (ifS == "if") . Left $ BadIf ifS s
      let f = Right . CondOp
      condOp <- case condOpS of
        "!=" -> f (/=)
        ">"  -> f (>)
        "==" -> f (==)
        ">=" -> f (>=)
        "<=" -> f (<=)
        "<"  -> f (<)
        _    -> Left $ BadCondOp condOpS s
      condVal <- case readMay condValS of
        Nothing -> Left $ BadCondVal valS s
        Just v -> Right $ Val v
      Right Instruction {..}
    _ -> Left $ BadLength s

lookupReg :: Reg -> Map Reg Val -> Val
lookupReg = fromMaybe 0 .: M.lookup

checkCond :: Map Reg Val -> Reg -> CondOp -> Val -> Bool
checkCond regs reg op = unCondOp op (lookupReg reg regs)

applyOp :: Map Reg Val -> Reg -> Op -> Val -> (Map Reg Val, Val)
applyOp regs reg op val = (M.insert reg newVal regs, newVal)
  where
    newVal = app (lookupReg reg regs)
    app = case op of
      Inc -> (+ val)
      Dec -> (+ negate val)

eval :: (Max Val, Map Reg Val) -> Instruction -> (Max Val, Map Reg Val)
eval (max, regs) Instruction{..} =
  (max <> Max newVal, regs')
  where
    (regs', newVal) =
      if checkCond regs from condOp condVal
        then applyOp regs to op val
        else (regs, 0)

evals :: Max Val -> Map Reg Val -> [Instruction] -> (Max Val, Map Reg Val)
evals max regs = foldl' eval (max, regs)

main :: IO ()
main = do
  cs :: Either Err [Instruction] <- traverse parser . lines <$> readFile "8input.txt"
  case cs of
    Left err -> error $ show err
    Right ins -> do
      let (max, regs) = evals (Max 0) mempty ins
      print . unVal . getMax $ max
      print . unVal . maximum $ regs

(.:) :: (Functor f, Functor f1) => (a -> b) -> f1 (f a) -> f1 (f b)
(.:) = fmap fmap fmap
