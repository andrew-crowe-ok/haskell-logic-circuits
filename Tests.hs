module Tests where

-- Import your Logic Modules
-- We verify Types, Gates, Circuits, and Utils
import Types
import Utils
import Circuits

-- We import Gates qualified so we don't confuse 
-- Circuit 'and' (Bit) with Haskell '&&' (Bool)
import qualified Gates as G

import Control.Monad (unless, forM_)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- TEST RUNNER UTILITIES
--------------------------------------------------------------------------------

-- Helper to print Passing checks in Green and Failing in Red
assert :: String -> Bool -> IO ()
assert name condition = do
    if condition
        then putStrLn $ "  [PASS] " ++ name
        else putStrLn $ "  [FAIL] " ++ name ++ " (Check implementation!)"

-- Helper to group tests
describe :: String -> IO () -> IO ()
describe groupName tests = do
    putStrLn $ "\n=== " ++ groupName ++ " ==="
    tests

--------------------------------------------------------------------------------
-- THE TEST SUITE
--------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Running Logic Circuit Verification Suite..."
    
    describe "Primitive Gates (Truth Tables)" $ do
        -- NAND Truth Table
        assert "NAND 0 0 -> 1" $ G.nand Zero Zero == One
        assert "NAND 0 1 -> 1" $ G.nand Zero One  == One
        assert "NAND 1 0 -> 1" $ G.nand One  Zero == One
        assert "NAND 1 1 -> 0" $ G.nand One  One  == Zero
        
        -- XOR Truth Table (Parity)
        assert "XOR  0 0 -> 0" $ G.xor Zero Zero == Zero
        assert "XOR  1 0 -> 1" $ G.xor One  Zero == One
        assert "XOR  0 1 -> 1" $ G.xor Zero One  == One
        assert "XOR  1 1 -> 0" $ G.xor One  One  == Zero
        
        -- NOT Truth Table
        assert "NOT  0   -> 1" $ G.not Zero == One
        assert "NOT  1   -> 0" $ G.not One  == Zero

    describe "Data Conversion (Endianness Checks)" $ do
        -- Check specific bit patterns (LSB First)
        -- 6 is 110 in binary. LSB First list should be [0, 1, 1]
        let bits6 = int2bit 6
        assert "int2bit 6 produces LSB-First [0,1,1]" $ bits6 == [Zero, One, One]
        
        -- Round Trip
        assert "Round Trip (42 -> Bits -> 42)" $ bit2intUnsigned (int2bit 42) == 42
        assert "Round Trip (0 -> Bits -> 0)"   $ bit2intUnsigned (int2bit 0) == 0

    describe "Adder Circuits (Arithmetic)" $ do
        -- Half Adder
        assert "Half Adder (1 + 1 -> Sum 0, Carry 1)" $ 
            halfAdder One One == [Zero, One]
            
        -- Full Adder
        assert "Full Adder (1 + 1 + 1 -> Sum 1, Carry 1)" $
            fullAdder One One One == [One, One]

        -- Ripple Adder (Specific Cases)
        let two   = int2bit 2
        let three = int2bit 3
        let five  = rippleAddN Zero two three
        
        assert "Ripple Adder (2 + 3 = 5)" $ bit2intUnsigned five == 5
        
        -- Check Overflow handling (15 + 1 = 16)
        -- 15 is [1,1,1,1]. Adding 1 should ripple all the way to [0,0,0,0,1]
        let fifteen = int2bit 15
        let one     = int2bit 1
        let sixteen = rippleAddN Zero fifteen one
        assert "Ripple Overflow (15 + 1 = 16)" $ bit2intUnsigned sixteen == 16

    describe "Property Tests (Automated Laws)" $ do
        -- Test 1: Commutativity (A + B == B + A)
        putStrLn "  Checking Commutativity for inputs 0..20..."
        let range = [0..20]
        let failures = [ (x, y) | x <- range, y <- range, 
                         let bx = int2bit x
                             by = int2bit y
                             sum1 = bit2intUnsigned (rippleAddN Zero bx by)
                             sum2 = bit2intUnsigned (rippleAddN Zero by bx)
                         in sum1 /= sum2 ]
                         
        assert "Addition is Commutative (order doesn't matter)" $ null failures

        -- Test 2: Identity (A + 0 == A)
        putStrLn "  Checking Identity (x + 0 == x) for inputs 0..50..."
        let idFailures = [ x | x <- [0..50],
                           let bx = int2bit x
                               b0 = int2bit 0
                               sum = bit2intUnsigned (rippleAddN Zero bx b0)
                           in sum /= x ]
                           
        assert "Addition Identity (adding zero changes nothing)" $ null idFailures

    putStrLn "\nTests Complete."