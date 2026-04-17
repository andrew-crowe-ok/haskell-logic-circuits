module Logic.CPU where

import Control.Monad.State
import Logic.Classes 
import Logic.Types (Bit(..), Byte(..), Memory, CpuState(..)) 
import Logic.Memory

initCpu :: Memory -> CpuState
initCpu initialMemory = CpuState {
  accumulator    = Byte (replicate 8 Zero),
  programCounter = 0,
  memory         = initialMemory
}