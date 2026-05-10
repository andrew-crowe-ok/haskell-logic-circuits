{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Layoutz 
import Byte    ( Byte(..), byteToIntUnsigned, byteToIntSigned, int2byteSigned )
import Bit     ( Bit(..) )
import Classes ( Arithmetic(add) )
import System.IO (hSetEncoding, hSetBuffering, hSetEcho, stdout, stdin, utf8, BufferMode(NoBuffering))

--------------------------------------------------------------------------------
-- 1. MODEL
--------------------------------------------------------------------------------

-- Reduced modes since binary conversion is now natively handled by the UI
data AppMode = UnsignedAdd | SignedAdd deriving (Eq, Show, Enum, Bounded)

-- Tracks which component currently receives keyboard input. 
-- SwA and SwB store the bit index (0 to 7) of the active switch cursor.
data Focus = NumA | SwA Int | NumB | SwB Int deriving (Eq, Show)

data AppModel = AppModel
    { byteA  :: Byte
    , byteB  :: Byte
    , strA   :: String
    , strB   :: String
    , mode   :: AppMode
    , focus  :: Focus
    }

-- Cycles the focus state forward
nextFocus :: Focus -> Focus
nextFocus NumA    = SwA 7 -- Start at MSB (index 7) when entering switches
nextFocus (SwA _) = NumB
nextFocus NumB    = SwB 7
nextFocus (SwB _) = NumA

-- Helper to interpret a Byte based on the current mode
getNumericValue :: AppMode -> Byte -> Int
getNumericValue UnsignedAdd b = byteToIntUnsigned b
getNumericValue SignedAdd b = byteToIntSigned b

-- Converts a typed string into a structured Byte. Defaults to 0 on parse failure.
syncFromStr :: String -> Byte
syncFromStr str = 
    case reads str :: [(Int, String)] of
        [(n, "")] -> int2byteSigned n
        _         -> int2byteSigned 0

-- Derives the numeric string representation of a Byte
syncFromByte :: AppMode -> Byte -> String
syncFromByte m b = show (getNumericValue m b)

-- Inverts the bit at the specified index
toggleBitAt :: Int -> Byte -> Byte
toggleBitAt idx (Byte bits) = 
    let (pre, b:post) = splitAt idx bits
        newB = if b == One then Zero else One
    in Byte (pre ++ newB : post)

--------------------------------------------------------------------------------
-- 2. ACTIONS 
--------------------------------------------------------------------------------

data AppAction
    = TypeChar Char
    | Backspace
    | CycleFocus
    | MoveCursor Int
    | ToggleBit
    | ToggleMode

--------------------------------------------------------------------------------
-- 3. UPDATE
--------------------------------------------------------------------------------

handleAction :: AppAction -> AppModel -> (AppModel, Cmd AppAction)
handleAction action model = case action of
    
    -- Handles numeric typing. Updates the string, then derives the new Byte.
    TypeChar c -> 
        let m' = case focus model of
                NumA -> 
                    let newStr = strA model ++ [c]
                    in model { strA = newStr, byteA = syncFromStr newStr }
                NumB -> 
                    let newStr = strB model ++ [c]
                    in model { strB = newStr, byteB = syncFromStr newStr }
                _ -> model
        in (m', CmdNone)
        
    -- Handles numeric deletion.
    Backspace -> 
        let safeInit "" = ""
            safeInit s  = init s
            m' = case focus model of
                NumA -> 
                    let newStr = safeInit (strA model)
                    in model { strA = newStr, byteA = syncFromStr newStr }
                NumB -> 
                    let newStr = safeInit (strB model)
                    in model { strB = newStr, byteB = syncFromStr newStr }
                _ -> model
        in (m', CmdNone)

    CycleFocus -> 
        (model { focus = nextFocus (focus model) }, CmdNone)
        
    -- Shifts the switch cursor left or right, clamping between bit indices 0 and 7.
    MoveCursor dir -> 
        let m' = case focus model of
                SwA idx -> model { focus = SwA (max 0 (min 7 (idx + dir))) }
                SwB idx -> model { focus = SwB (max 0 (min 7 (idx + dir))) }
                _       -> model
        in (m', CmdNone)

    -- Toggles the active switch. Updates the Byte, then derives the new numeric string.
    ToggleBit -> 
        let m' = case focus model of
                SwA idx -> 
                    let newByte = toggleBitAt idx (byteA model)
                    in model { byteA = newByte, strA = syncFromByte (mode model) newByte }
                SwB idx -> 
                    let newByte = toggleBitAt idx (byteB model)
                    in model { byteB = newByte, strB = syncFromByte (mode model) newByte }
                _ -> model
        in (m', CmdNone)

    -- Changes operation mode and re-evaluates the string representations
    ToggleMode -> 
        let newMode = if mode model == UnsignedAdd then SignedAdd else UnsignedAdd
            m' = model 
                { mode = newMode
                , strA = syncFromByte newMode (byteA model)
                , strB = syncFromByte newMode (byteB model)
                }
        in (m', CmdNone)

--------------------------------------------------------------------------------
-- 4. SUBSCRIPTIONS
--------------------------------------------------------------------------------

handleSubs :: AppModel -> Sub AppAction
handleSubs _ = subKeyPress $ \key -> case key of
    KeyChar 'm'  -> Just ToggleMode
    KeyChar ' '  -> Just ToggleBit
    KeyChar c    -> if c `elem` ("0123456789-" :: String) then Just (TypeChar c) else Nothing
    KeyBackspace -> Just Backspace
    KeyTab       -> Just CycleFocus
    -- Because the list is LSB-first (index 0) but visually rendered MSB-first,
    -- moving Left visually means moving to a higher bit index (+1).
    KeyLeft      -> Just (MoveCursor 1)   
    KeyRight     -> Just (MoveCursor (-1)) 
    _            -> Nothing

--------------------------------------------------------------------------------
-- 5. VIEW
--------------------------------------------------------------------------------

-- Formats a list of bits as ASCII LEDs, reversing to display MSB on the left.
renderBits :: [Bit] -> String
renderBits bits = unwords $ map (\b -> if b == One then "(O)" else "( )") (reverse bits)

-- Formats the switches and highlights the cursor if the row is currently focused.
renderSwitches :: Focus -> String -> [Bit] -> String
renderSwitches currentFocus target bits = 
    let activeIdx = case currentFocus of
            SwA i | target == "A" -> i
            SwB i | target == "B" -> i
            _ -> -1
        msbFirst = reverse bits
        
        -- Normal switches use [ ], focused switch uses { }
        strs = [ (if activeIdx == i then "{" else "[") ++ 
                 (if b == One then "X" else " ") ++ 
                 (if activeIdx == i then "}" else "]") 
               | (i, b) <- zip [7,6..0] msbFirst ]
    in unwords strs

renderView :: AppModel -> L
renderView model = 
    -- Unpack the bytes for rendering
    let (Byte bitsA) = byteA model
        (Byte bitsB) = byteB model
        resByte = add (byteA model) (byteB model)
        (Byte resBits) = resByte
    in layout
    [ box "Haskell Logic Circuit Simulator"
        [ text $ "Mode: " ++ show (mode model)
        , text "-----------------------------------------"
        
        , text "[ Register A ]"
        , text $ "  LEDs:     " ++ renderBits bitsA
        , text $ "  Switches: " ++ renderSwitches (focus model) "A" bitsA
        , row [ text $ if focus model == NumA then "  Numeric: >" else "  Numeric:  "
              , let val = strA model ++ if focus model == NumA then "_" else "" 
                in text (if null val then " " else val) 
              ]
        , text " "
        
        , text "[ Register B ]"
        , text $ "  LEDs:     " ++ renderBits bitsB
        , text $ "  Switches: " ++ renderSwitches (focus model) "B" bitsB
        , row [ text $ if focus model == NumB then "  Numeric: >" else "  Numeric:  "
              , let val = strB model ++ if focus model == NumB then "_" else "" 
                in text (if null val then " " else val) 
              ]
        , text "-----------------------------------------"
        
        , text "[ ALU Output ]"
        , text $ "  LEDs:     " ++ renderBits resBits
        , text $ "  Numeric:  " ++ show (getNumericValue (mode model) resByte)
        ]
    , br
    , ul [ "Controls:"
         , "  [Tab]         Switch Focus (Num A -> Sw A -> Num B -> Sw B)"
         , "  [Left/Right]  Move Switch Cursor (when focused on switches)"
         , "  [Space]       Toggle Switch      (when focused on switches)"
         , "  [m]           Change Mode"
         , "  [ESC]         Quit Simulator"
         ]
    ]

--------------------------------------------------------------------------------
-- 6. MAIN APP RUNNER
--------------------------------------------------------------------------------

logicSimApp :: LayoutzApp AppModel AppAction
logicSimApp = LayoutzApp
    { appInit          = (AppModel (int2byteSigned 0) (int2byteSigned 0) "0" "0" UnsignedAdd NumA, CmdNone)
    , appUpdate        = handleAction
    , appSubscriptions = handleSubs
    , appView          = renderView
    }

main :: IO ()
main = do
    -- Encoding and Buffering explicitly set to support raw terminal inputs on WSL
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    runApp logicSimApp
