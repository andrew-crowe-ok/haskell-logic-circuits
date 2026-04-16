{-
Contains custom data types for simulation of binary data.
-}

module Logic.Types where

-- Define the typeclass interface
class Arithmetic a where
    add :: a -> a -> a
    sub :: a -> a -> a

class Sequential a where
  update :: Bit -> Bit -> a -> a

-- [Bit] are always ordered from LSB on the left to MSB on the right
data Bit = Zero | One 
    deriving (Show, Eq)

-- A distinct type for an 8-bit Word. 
-- It wraps the raw list so you can't accidentally use it as a variable-length list.
newtype Byte = Byte [Bit] 
    deriving (Show, Eq)