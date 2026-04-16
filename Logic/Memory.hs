module Logic.Memory where

import Logic.Types (Bit(..), Byte(..))
import Data.Sequence (Seq)

instance Sequential srLatch where
  update s r (srLatch q) =
      srLatch (srLatch s r q)

srLatch :: Bit -> Bit -> Bit -> Bit
srLatch One Zero _  = One
srLatch Zero One _  = Zero
srLatch Zero Zero q = q
srLatch One One _   = Zero

dFlipFlop :: Bit -> Bit -> Bit -> Bit
dFlipFlop d clock q
  | clock == One = d
  | otherwise    = q
