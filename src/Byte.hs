module Byte where

import Bit ( Bit(..) )
import Classes ( Arithmetic(..) )
import Circuits (rippleAddN)
import Utils (int2bit, bit2intUnsigned)
import qualified Gates as G

newtype Byte = Byte [Bit] 
  deriving (Show, Eq)

instance Arithmetic Byte where
  add (Byte a) (Byte b) = 
    let rawSum = rippleAddN Zero a b
    in Byte (setLength rawSum)
  sub a b = add a (negateByte b)

width :: Int
width = 8

--------------------------------------------------------------------------------
-- PRIVATE HELPERS
--------------------------------------------------------------------------------

-- Ensures a list is exactly 8 bits (Pad LSBs with Zero if short, truncate if long)
-- Note: In this system, LSB is at the head (index 0).
setLength :: [Bit] -> [Bit]
setLength bits = 
  let len = length bits
  in if len < width
      then bits ++ replicate (width - len) Zero -- Pad end (MSB side in this LSB-first system)
      else take width bits                      -- Truncate

-- Extract raw bits (Unwrap)
getBits :: Byte -> [Bit]
getBits (Byte bits) = bits

--------------------------------------------------------------------------------
-- CONSTRUCTORS (Int <-> Byte)
--------------------------------------------------------------------------------

int2byteSigned :: Int -> Byte
int2byteSigned n
  | n >= 0    = case int2bit n of
    Just bits -> Byte $ setLength bits
    Nothing   -> Byte $ replicate width Zero
  | otherwise = case int2bit (abs n) of
    Just posBits ->
      let inverted = map G.not $ setLength posBits
          rawPlus1 = rippleAddN Zero inverted [One]
      in Byte $ setLength rawPlus1
    Nothing -> Byte $ replicate width Zero


-- Unsigned Interpretation (Treats raw bits as 0 to 255)
byteToIntUnsigned :: Byte -> Int
byteToIntUnsigned (Byte bits) = bit2intUnsigned bits

-- Signed Interpretation (Treats raw bits as -128 to 127)
byteToIntSigned :: Byte -> Int
byteToIntSigned (Byte bits) = 
  let 
    -- In LSB-first, the last bit is the MSB (Sign Bit)
    msb = last bits
    magnitudeBits = init bits 
    magnitudeVal  = bit2intUnsigned magnitudeBits
    
    -- If MSB is 1, subtract 2^7 (128). If 0, add 0.
    signVal = if msb == One then -128 else 0
  in 
    magnitudeVal + signVal

--------------------------------------------------------------------------------
-- LOGIC OPERATIONS
--------------------------------------------------------------------------------

bitwiseNot :: Byte -> Byte
bitwiseNot (Byte bits) = Byte (map G.not bits)

-- Calculates -x using 2's complement (Invert + 1)
negateByte :: Byte -> Byte
negateByte b = add (bitwiseNot b) (int2byteSigned 1)