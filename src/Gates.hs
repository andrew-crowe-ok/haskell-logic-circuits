{-
Contains simulated primitive logic gates.
-}

module Gates where

import Prelude hiding (not, and, or)
import Bit


-- 2-input Gates

nand :: Bit -> Bit -> Bit
nand One One = Zero
nand _   _   = One

not :: Bit -> Bit
not x = nand x x

and :: Bit -> Bit -> Bit
and x y = not (nand x y)

or :: Bit -> Bit -> Bit
or x y = nand (not x) (not y)

xor :: Bit -> Bit -> Bit
xor x y = and (or x y) (nand x y)


-- n-input Gates

andN :: [Bit] -> Bit
andN = foldr and One

orN :: [Bit] -> Bit
orN = foldr or Zero

nandN :: [Bit] -> Bit
nandN inputs = not (andN inputs)

xorN :: [Bit] -> Bit
xorN = foldr xor Zero
