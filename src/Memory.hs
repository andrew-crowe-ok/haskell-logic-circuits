{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Memory where

import Classes
import Bit ( Bit(..) )    -- ADD THIS LINE
import Byte ( Byte(..) )  -- ADD THIS LIN
import qualified Gates as G

newtype SrLatch = SrLatch Bit deriving (Show, Eq)
newtype DFlipFlop = DFlipFlop Bit deriving (Show, Eq)
newtype Register8 = Register8 Byte deriving (Show, Eq)

-- An SR Latch takes a Set bit and a Reset bit.
instance Sequential (Bit, Bit) SrLatch where
  update (s, r) (SrLatch qPrev) = SrLatch qNext
    where
      qNot  = G.not (G.or s qPrev)
      qNext = G.not (G.or r qNot)

-- A D Flip-Flop takes a Data bit and a Clock bit.
instance Sequential (Bit, Bit) DFlipFlop where
  update (d, clock) (DFlipFlop qPrev) =
    let s = G.and d clock
        r = G.and (G.not d) clock
        (SrLatch qNext) = update (s, r) (SrLatch qPrev)
    in DFlipFlop qNext

-- An 8-Bit Register takes a Data Byte and a Clock bit.
instance Sequential (Byte, Bit) Register8 where
  update (Byte dBits, clock) (Register8 (Byte qBits)) =
    let applyDff d q = let (DFlipFlop res) = update (d, clock) (DFlipFlop q)
                       in res 
        newBits = zipWith applyDff dBits qBits
    in Register8 (Byte newBits)