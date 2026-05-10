{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Layoutz 
import Byte    ( byteToIntUnsigned, byteToIntSigned, int2byteSigned )
import Utils   ( int2bit, parseBinaryString, bit2string, bit2intUnsigned )
import Classes ( Arithmetic(add) )
import System.IO (hSetEncoding, hSetBuffering, hSetEcho, stdout, stdin, utf8, BufferMode(NoBuffering))

--------------------------------------------------------------------------------
-- 1. MODEL
--------------------------------------------------------------------------------

data AppMode = UnsignedAdd | SignedAdd | DecToBin | BinToDec 
    deriving (Eq, Show, Enum, Bounded)

data ActiveField = FieldA | FieldB 
    deriving (Eq, Show)

data AppModel = AppModel
    { inputA      :: String
    , inputB      :: String
    , result      :: String
    , mode        :: AppMode
    , activeField :: ActiveField
    }

nextMode :: AppMode -> AppMode
nextMode m = if m == maxBound then minBound else succ m

--------------------------------------------------------------------------------
-- 2. ACTIONS 
--------------------------------------------------------------------------------

data AppAction
    = TypeChar Char
    | Backspace
    | ToggleMode
    | ToggleFocus
    | Calculate

--------------------------------------------------------------------------------
-- 3. UPDATE
--------------------------------------------------------------------------------

-- Renamed from appUpdate to handleAction to avoid naming collision
handleAction :: AppAction -> AppModel -> (AppModel, Cmd AppAction)
handleAction action model = case action of
    TypeChar c -> 
        let m' = case activeField model of
                FieldA -> model { inputA = inputA model ++ [c] }
                FieldB -> model { inputB = inputB model ++ [c] }
        in (m', CmdNone)
        
    Backspace -> 
        let safeInit "" = ""
            safeInit s  = init s
            m' = case activeField model of
                FieldA -> model { inputA = safeInit (inputA model) }
                FieldB -> model { inputB = safeInit (inputB model) }
        in (m', CmdNone)

    ToggleMode -> 
        (model { mode = nextMode (mode model), result = "" }, CmdNone)
        
    ToggleFocus -> 
        let nextF = if activeField model == FieldA then FieldB else FieldA
        in (model { activeField = nextF }, CmdNone)

    Calculate -> 
        (model { result = performCalculation model }, CmdNone)

performCalculation :: AppModel -> String
performCalculation model = case mode model of
    UnsignedAdd -> 
        case (reads (inputA model) :: [(Int, String)], reads (inputB model) :: [(Int, String)]) of
            ([(a, "")], [(b, "")]) -> 
                let res = add (int2byteSigned a) (int2byteSigned b)
                in show (byteToIntUnsigned res)
            _ -> "Error: Invalid integer input"
            
    SignedAdd -> 
        case (reads (inputA model) :: [(Int, String)], reads (inputB model) :: [(Int, String)]) of
            ([(a, "")], [(b, "")]) -> 
                let res = add (int2byteSigned a) (int2byteSigned b)
                in show (byteToIntSigned res)
            _ -> "Error: Invalid integer input"
            
    DecToBin -> 
        case reads (inputA model) :: [(Int, String)] of
            ([(n, "")]) -> case int2bit n of
                Just bits -> reverse $ bit2string bits
                Nothing   -> "Error: Input must be >= 0"
            _ -> "Error: Invalid integer input"
            
    BinToDec -> 
        case parseBinaryString (inputA model) of
            Just bits -> show (bit2intUnsigned bits)
            Nothing   -> "Error: Invalid binary string"

--------------------------------------------------------------------------------
-- 4. SUBSCRIPTIONS
--------------------------------------------------------------------------------

-- Renamed to handleSubs for consistency
handleSubs :: AppModel -> Sub AppAction
handleSubs _ = subKeyPress $ \key -> case key of
    KeyChar 'm'  -> Just ToggleMode
    KeyChar c    -> if c `elem` ("0123456789-" :: String) then Just (TypeChar c) else Nothing
    KeyBackspace -> Just Backspace
    KeyTab       -> Just ToggleFocus
    KeyEnter     -> Just Calculate
    _            -> Nothing

--------------------------------------------------------------------------------
-- 5. VIEW
--------------------------------------------------------------------------------

renderView :: AppModel -> L
renderView model = layout
    [ box "Haskell Logic Circuit Simulator"
        [ text $ "Mode: " ++ show (mode model)
        , text "-----------------------------------------"
        
        , row 
            [ text $ if activeField model == FieldA then "> Input A: " else "  Input A: "
            , let valA = inputA model ++ if activeField model == FieldA then "_" else ""
              in text (if null valA then " " else valA)
            ]
            
        , if mode model `elem` [DecToBin, BinToDec]
            then text " " -- Fixed: Replaced "" with " "
            else row 
                [ text $ if activeField model == FieldB then "> Input B: " else "  Input B: "
                , let valB = inputB model ++ if activeField model == FieldB then "_" else ""
                  in text (if null valB then " " else valB) -- Fixed: Prevents empty text node
                ]
                
        , text "-----------------------------------------"
        , text $ "Result: " ++ result model
        ]
    , br
    , ul [ "Controls:"
         , "  [Tab]   Switch Input Field"
         , "  [m]     Change Mode"
         , "  [Enter] Calculate"
         , "  [ESC]   Quit Simulator"
         ]
    ]

--------------------------------------------------------------------------------
-- 6. MAIN APP RUNNER
--------------------------------------------------------------------------------

logicSimApp :: LayoutzApp AppModel AppAction
logicSimApp = LayoutzApp
    { appInit          = (AppModel "" "" "" UnsignedAdd FieldA, CmdNone)
    , appUpdate        = handleAction
    , appSubscriptions = handleSubs
    , appView          = renderView
    }

main :: IO ()
main = do
    -- 1. Set Encoding
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    
    -- 2. Disable Windows Input Buffering
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    
    -- 3. Run App
    runApp logicSimApp