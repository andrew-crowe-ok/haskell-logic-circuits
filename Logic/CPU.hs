module Logic.CPU where

import Control.Monad.State
import Logic.Types

type Memory = [(Int, Byte)]


data CpuState = CpuState {
  accumulator    :: Byte,   -- The main register for arithmetic and logic operations
  programCounter :: Int,    -- The address of the next instruction to be executed
  memory         :: Memory  -- The memory contents
} deriving (Eq, Show)

initCpu :: Memory -> CpuState
initCpu initialMemory = CpuState {
  accumulator    = Byte (replicate 8 Zero),
  programCounter = 0,
  memory         = initialMemory
}