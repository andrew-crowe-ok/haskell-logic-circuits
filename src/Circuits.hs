{-
Contains basic arithmetic circuits built from simulated gates.
-}

module Circuits where

import Prelude hiding ( not, and, or )
import Bit ( Bit(Zero) )
import Gates ( not, and, or, xor )


-- Uses XOR and AND to generate a sum bit and carry bit. Outputs [sum, carry].
halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder x y = (xor x y, and x y)


-- Uses halfAdder and an OR gate to handle 2-bit sums with carries. Outputs [sum, carry].
fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder x y cIn =
  let
    (s1, c1)   = halfAdder x y
    (sOut, c2) = halfAdder s1 cIn
    cOut       = or c1 c2
  in
    (sOut, cOut)


-- Accepts a carry bit and two lists of n Bits, recursively generates their [Bit] sum.
-- Unsafe - no checks or limits on the size of the input lists.
rippleAddN :: Bit -> [Bit] -> [Bit] -> [Bit]
rippleAddN c [] (y:ys)     = rippleAddN c [Zero] (y:ys)
rippleAddN c (x:xs) []     = rippleAddN c (x:xs) [Zero]
rippleAddN c [] []         = [c]
rippleAddN c (x:xs) (y:ys) =
  let (sN, cN) = fullAdder x y c
      recurse  = rippleAddN cN xs ys
  in  sN : recurse




---- CIRCUITS NOT CURRENTLY IN USE

mux :: Bit -> Bit -> Bit -> Bit
mux x y s =
  let and1 = and x $ not s
      and2 = and s y
  in or and1 and2


crossbar :: Bit -> Bit -> Bit -> (Bit, Bit)
crossbar x1 x2 s =
  let y1 = mux x1 x2 s
      y2 = mux x2 x1 s
  in (y1, y2)