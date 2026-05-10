module CPU where

import Types (Memory, CpuState(..)) 
import Bit   ( Bit(..) )  
import Byte  ( Byte(..) )  

initCpu :: Memory -> CpuState
initCpu initialMemory = CpuState {
  accumulator    = Byte (replicate 8 Zero),
  programCounter = 0,
  memory         = initialMemory
}