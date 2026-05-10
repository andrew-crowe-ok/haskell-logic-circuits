{-
A simulation of logic circuits, from nand to a ripple-carry adder.
-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{- HLINT ignore "Use when" -}

module Main where

import Prelude hiding (not, and, or) 
import System.IO      (hFlush, stdout)
import System.Process (callCommand)

import Bit
import Gates
import Circuits
import Classes
import Utils
import Byte
import Types


-- Entry point
main :: IO ()
main = do
  putStrLn "\n============================================"
  putStrLn "      HASKELL LOGIC CIRCUIT SIMULATOR"
  putStrLn "============================================"
  menuLoop 


-- The Recursive Menu Loop
menuLoop :: IO ()
menuLoop = do
  putStrLn "\nSELECT AN OPERATION:"
  putStrLn "1. Convert Decimal to Binary"
  putStrLn "2. Convert Unsigned Binary to Decimal"
  putStrLn "3. Unsigned Addition (0 to 255)"     
  putStrLn "4. Signed Addition (-128 to 127)"   
  putStrLn "5. View Utility Function Reference (Help)"
  putStrLn "6. Enter function mode (REPL)"
  putStrLn "7. Exit simulation"
  
  putStr ">> "
  hFlush stdout
  
  choice <- getLine
  case choice of
    "1" -> runDecToBin    >> menuLoop
    "2" -> runBinToDec    >> menuLoop
    "3" -> runUnsignedAdd >> menuLoop
    "4" -> runSignedAdd   >> menuLoop
    "5" -> runHelp        >> menuLoop
    "6" -> do
        putStrLn "\nLoading function mode... (Type ':quit' to return to menu)"
        callCommand "cabal repl logic-sim"
        menuLoop
    "7" -> putStrLn "Exiting simulation. Goodbye!"
    _   -> putStrLn "Invalid selection. Please try again." >> menuLoop


-------------------------------------------------------------------------
-- MENU ACTIONS
-- These "Wrapper Functions" handle the IO and call your pure logic
-------------------------------------------------------------------------

-- Option 1: Decimal -> Binary
runDecToBin :: IO ()
runDecToBin = do
    putStr "Enter a non-negative decimal number (e.g., 42): "
    hFlush stdout
    input <- getLine
    let n = read input :: Int
    
    case int2bit n of
      Just bits -> do
        let output = reverse $ bit2string bits
        putStrLn $ "Binary Output: " ++ output
      Nothing -> 
        putStrLn "Error: Invalid input. Please enter a non-negative integer."


-- Option 2: Binary -> Decimal
runBinToDec :: IO ()
runBinToDec = do
    putStr "Enter a binary string (e.g., 101010): "
    hFlush stdout
    input <- getLine
    
    case parseBinaryString input of
      Just bits -> do
        let n = bit2intUnsigned bits
        putStrLn $ "Decimal Output: " ++ show n
      Nothing ->
        putStrLn "Error: Invalid binary string containing non-binary characters."


-- Option 3: Unsigned Addition (Wraps at 255)
runUnsignedAdd :: IO ()
runUnsignedAdd = do
  putStrLn "\n--- Unsigned Mode (0 to 255) ---"
  putStr "Enter first number: "
  hFlush stdout
  inputA <- getLine
  
  putStr "Enter second number: "
  hFlush stdout
  inputB <- getLine
  
  let a = read inputA :: Int
  let b = read inputB :: Int
  
  -- 1. ENCODE: Convert Int to 8-bit Byte
  -- We use int2byteSigned generically because it just fills bits
  let byteA = int2byteSigned a
  let byteB = int2byteSigned b
  
  -- 2. CIRCUIT: Run the Adder
  -- This uses rippleAddN internally and truncates to 8 bits
  let resultByte = add byteA byteB
  
  -- 3. DECODE: Interpret result as Unsigned
  let resultInt = byteToIntUnsigned resultByte
  
  -- Unwrapping, reversing, then showing the result
  putStrLn $ "Binary Calculation: " ++ show (let (Byte bits) = byteA in Byte (reverse bits)) 
                                    ++ " + " ++ 
                                    show (let (Byte bits) = byteB in Byte (reverse bits))
  putStrLn $ "Int/Binary Result: " ++ show resultInt ++ " / " ++ show 
  
  -- Check for visual overflow (if logic result != math result)
  if (a + b) /= resultInt 
  then putStrLn "NOTE: Overflow occurred (Result > 255 wrapped around)"
  else return ()


-- Option 4: Signed Addition (Wraps at 127/-128)
runSignedAdd :: IO ()
runSignedAdd = do
  putStrLn "\n--- Signed Mode (-128 to 127) ---"
  putStr "Enter first number: "
  hFlush stdout
  inputA <- getLine
  
  putStr "Enter second number: "
  hFlush stdout
  inputB <- getLine
  
  let a = read inputA :: Int
  let b = read inputB :: Int
  
  -- 1. ENCODE: Convert Int to 8-bit Byte
  -- Handles negative inputs by creating 2's complement bits
  let byteA = int2byteSigned a
  let byteB = int2byteSigned b
  
  -- 2. CIRCUIT: Run the Adder
  -- EXACT SAME CIRCUIT as Unsigned!
  let resultByte = add byteA byteB
  
  -- 3. DECODE: Interpret result as Signed (Check MSB)
  let resultInt = byteToIntSigned resultByte
  
  putStrLn $ "Binary Calculation: " ++ show (let (Byte bits) = byteA in Byte (reverse bits)) 
                                    ++ " + " ++ 
                                    show (let (Byte bits) = byteB in Byte (reverse bits))
  putStrLn $ "Result: " ++ show resultInt 
  
  -- Check for overflow
  if (a + b) /= resultInt
    then putStrLn "NOTE: Overflow occurred (Result went out of -128..127 range)"
    else return ()
  

-- Option 5: HELP / Reference Guide
runHelp :: IO ()
runHelp = do
  putStrLn "\n=============================="
  putStrLn "  UTILITY FUNCTION REFERENCE    "
  putStrLn "=============================="
  putStrLn "\nTo access these functions directly, press Enter to leave this screen, then option 6 to enter function mode."
  putStrLn "Note: 'Bit List' implies LSB-First order (e.g., [1, 2, 4, 8...])."
  putStrLn "------------------------------------------------------------"
  
  putStrLn ""
  putStrLn "1. int2bit :: Int -> Maybe [Bit]"
  putStrLn "   Converts a decimal integer to a list of Bits."
  putStrLn "   Usage: int2bit 6  ->  [Zero, One, One]"
  putStrLn ""

  putStrLn "2. parseBinaryString :: String -> Maybe [Bit]"
  putStrLn "   Parses a string of '1's and '0's into a list of Bits."
  putStrLn "   Usage: parseBinaryString \"110\"  ->  Just [Zero, One, One]"
  putStrLn ""

  putStrLn "3. bit2intUnsigned :: [Bit] -> Int"
  putStrLn "   Converts a list of Bits to a non-negative decimal integer."
  putStrLn "   Usage: bit2int [Zero, One]  ->  2"
  putStrLn ""

  putStrLn "4. bit2string :: [Bit] -> String"
  putStrLn "    Formats a Bit list into a raw string (LSB-Left)."
  putStrLn "    Usage: bit2string [Zero, One]  ->  \"01\""
  
  putStrLn ""
  putStrLn "=================================="
  putStrLn "Press Enter to return to menu..."
  _ <- getLine
  return ()