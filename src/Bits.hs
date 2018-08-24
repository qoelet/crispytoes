{-# LANGUAGE LambdaCase #-}

module Bits where

import           Data.Char (chr, digitToInt, intToDigit, isDigit, ord)
import           Numeric (readInt, showIntAtBase)

import           Boolean

newtype Bits = Bits String
  deriving (Eq, Show)

charToBits :: Char -> Bits
charToBits c = Bits bs'
  where
    bs = showIntAtBase 2 intToDigit (ord c) ""
    bs' = if length bs < 7 then "0" ++ bs else bs

bitsToChar :: Bits -> Char
bitsToChar (Bits xs)
  = chr . fst . head $
    readInt 2 isDigit digitToInt xs

bitsToBooleans :: Bits -> [Boolean]
bitsToBooleans (Bits xs)
  = map (
      \case
        '1' -> T
        _ -> F
      ) xs

booleansToBits :: [Boolean] -> Bits
booleansToBits xs
  = Bits $ map (
      \case
        T -> '1'
        _ -> '0'
      ) xs

otp :: [Boolean] -> [[Boolean]] -> [[Boolean]]
otp k = map (zipWith bXor k)
