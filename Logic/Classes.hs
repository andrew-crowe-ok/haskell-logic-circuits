module Logic.Classes where

class Arithmetic a where
    geminiAdd :: a -> a -> a
    geminiSub :: a -> a -> a

-- 'input' represents the signals entering the circuit (e.g., Data, Clock)
-- 'state' represents the memory holding the data
class Sequential input state where
  update :: input -> state -> state