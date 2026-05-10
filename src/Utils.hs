{-
Contains a variety of functions designed to test the circuitry 
in different ways and with different data types.
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Utils where

import Prelude hiding (not, and, or)
import Bit ( Bit(..) )


parseBinaryString :: String -> Maybe [Bit]
parseBinaryString [] = Nothing
parseBinaryString str = go str []
  where
    go [] acc = Just acc
    go ('0':cs) acc = go cs (Zero : acc)
    go ('1':cs) acc = go cs (One : acc)
    go _ _ = Nothing


-------- TYPE CONVERTERS


-- Takes a non-negative decimal integer and produces a list of Bits.
int2bit :: Int -> Maybe [Bit]
int2bit n
  | n < 0     = Nothing
  | n == 0    = Just [Zero]
  | otherwise = Just $ unfold (== 0) convertBit (`div` 2) n
  where
    convertBit x = if x `mod` 2 == 1 then One else Zero


-- Takes an unsigned list of Bits and produces a string.
bit2string :: [Bit] -> String
bit2string = foldr go []
  where
    go b recur
      | b == Zero = '0' : recur
      | b == One  = '1' : recur


-- Takes an unsigned list of n Bits and produces a decimal integer.
bit2intUnsigned :: [Bit] -> Int
bit2intUnsigned bits = sum $ zipWith (*) values powers
  where
    toVal Zero = 0
    toVal One  = 1
    values     = map toVal bits
    powers     = iterate (*2) 1


-- Takes a signed list of n Bits and produces a decimal integer.
bit2intSigned :: [Bit] -> Int
bit2intSigned bits =
  let
    msb           = last bits
    magnitudeBits = init bits
    magnitudeVal  = bit2intUnsigned magnitudeBits
    signVal       = if msb == One
                      then -(2 ^ (length bits - 1))
                      else 0
  in
    magnitudeVal + signVal




-------- HELPER FUNCTIONS


-- Checks the sign bit.
isNegative :: [Bit] -> Bool
isNegative bits
  | last bits == One = True
  | otherwise        = False


-- Uses an initial value and a predicate to generate a list.
unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)