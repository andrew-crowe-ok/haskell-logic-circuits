{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Byte ( Byte(..) )

type Memory = [(Int, Byte)]

data CpuState = CpuState {
  accumulator    :: Byte,   -- The main register for arithmetic and logic operations
  programCounter :: Int,    -- The address of the next instruction to be executed
  memory         :: Memory  -- The memory contents
} deriving (Eq, Show)