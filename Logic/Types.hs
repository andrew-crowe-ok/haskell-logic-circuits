{-# LANGUAGE MultiParamTypeClasses #-}

module Logic.Types where

data Bit = Zero | One 
    deriving (Show, Eq)

newtype Byte = Byte [Bit] 
    deriving (Show, Eq)

type Memory = [(Int, Byte)]

data CpuState = CpuState {
  accumulator    :: Byte,   -- The main register for arithmetic and logic operations
  programCounter :: Int,    -- The address of the next instruction to be executed
  memory         :: Memory  -- The memory contents
} deriving (Eq, Show)