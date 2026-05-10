{-# LANGUAGE MultiParamTypeClasses #-}

module Classes(Arithmetic(..), Sequential(..)) where

class Arithmetic a where
  add :: a -> a -> a
  sub :: a -> a -> a

-- 'input' represents the signals entering the circuit (e.g., Data, Clock)
-- 'state' represents the memory holding the data
class Sequential input state where
  update :: input -> state -> state