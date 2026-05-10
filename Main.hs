{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Layoutz
import Byte    ( Byte(..), byteToIntUnsigned, byteToIntSigned, int2byteSigned )
import Bit     ( Bit(..) )
import Classes ( Arithmetic(add) )
import System.IO (hSetEncoding, hSetBuffering, hSetEcho, stdout, stdin, utf8, BufferMode(..))
import System.Exit (exitFailure)
import qualified System.Console.Terminal.Size as Term (size, Window(width, height))

--------------------------------------------------------------------------------
-- 1. MODEL
--------------------------------------------------------------------------------

data AppMode = UnsignedAdd | SignedAdd deriving (Eq, Enum, Bounded)

instance Show AppMode where
    show UnsignedAdd = "Unsigned Addition"
    show SignedAdd   = "Signed Addition"

data Focus = NumA | SwA Int | NumB | SwB Int deriving (Eq, Show)

data AppModel = AppModel
    { byteA          :: Byte
    , byteB          :: Byte
    , strA           :: String
    , strB           :: String
    , mode           :: AppMode
    , focus          :: Focus
    , frame          :: Int
    }

checkOverflow :: AppMode -> Byte -> Byte -> Bool
checkOverflow UnsignedAdd a b =
    (byteToIntUnsigned a + byteToIntUnsigned b) > 255
checkOverflow SignedAdd a b =
    let valA = byteToIntSigned a
        valB = byteToIntSigned b
        sumVal = valA + valB
    in sumVal < -128 || sumVal > 127

getNumericValue :: AppMode -> Byte -> Int
getNumericValue UnsignedAdd b = byteToIntUnsigned b
getNumericValue SignedAdd b = byteToIntSigned b

syncFromStr :: String -> Byte
syncFromStr str =
    case reads str :: [(Int, String)] of
        [(n, "")] -> int2byteSigned n
        _         -> int2byteSigned 0

syncFromByte :: AppMode -> Byte -> String
syncFromByte m b = show (getNumericValue m b)

setBitAt :: Int -> Bit -> Byte -> Byte
setBitAt idx newBit (Byte bits) =
    case splitAt idx bits of
        (pre, _:post) -> Byte (pre ++ newBit : post)
        _             -> Byte bits

--------------------------------------------------------------------------------
-- 2. ACTIONS
--------------------------------------------------------------------------------

data AppAction
    = TypeChar Char
    | Backspace
    | MoveCursor Int
    | AdjustValue Int
    | ToggleMode
    | AnimTick
    | Quit

--------------------------------------------------------------------------------
-- 3. UPDATE
--------------------------------------------------------------------------------

handleAction :: AppAction -> AppModel -> (AppModel, Cmd AppAction)
handleAction action model = case action of
    Quit             -> (model, CmdExit)
    AnimTick         -> (model { frame = frame model + 1 }, CmdNone)

    MoveCursor dir   ->
        let nextFocus = case (focus model, dir) of
                (NumA, -1)  -> SwA 7
                (SwA i, -1) -> if i > 0 then SwA (i-1) else NumB
                (NumB, -1)  -> SwB 7
                (SwB i, -1) -> if i > 0 then SwB (i-1) else NumA
                (NumA, 1)   -> SwB 0
                (SwB i, 1)  -> if i < 7 then SwB (i+1) else NumB
                (NumB, 1)   -> SwA 0
                (SwA i, 1)  -> if i < 7 then SwA (i+1) else NumA
                _           -> focus model
        in (model { focus = nextFocus }, CmdNone)

    AdjustValue delta ->
        let m' = case focus model of
                NumA -> let current = getNumericValue (mode model) (byteA model)
                            newVal = current + delta
                            clamped = case mode model of
                                UnsignedAdd -> max 0 (min 255 newVal)
                                SignedAdd   -> max (-128) (min 127 newVal)
                            newByte = int2byteSigned clamped
                        in model { byteA = newByte, strA = syncFromByte (mode model) newByte }
                NumB -> let current = getNumericValue (mode model) (byteB model)
                            newVal = current + delta
                            clamped = case mode model of
                                UnsignedAdd -> max 0 (min 255 newVal)
                                SignedAdd   -> max (-128) (min 127 newVal)
                            newByte = int2byteSigned clamped
                        in model { byteB = newByte, strB = syncFromByte (mode model) newByte }
                SwA i -> let targetBit = if delta > 0 then One else Zero
                             newByte = setBitAt i targetBit (byteA model)
                         in model { byteA = newByte, strA = syncFromByte (mode model) newByte }
                SwB i -> let targetBit = if delta > 0 then One else Zero
                             newByte = setBitAt i targetBit (byteB model)
                         in model { byteB = newByte, strB = syncFromByte (mode model) newByte }
        in (m', CmdNone)

    TypeChar c ->
        let m' = case focus model of
                NumA -> let ns = strA model ++ [c] in model { strA = ns, byteA = syncFromStr ns }
                NumB -> let ns = strB model ++ [c] in model { strB = ns, byteB = syncFromStr ns }
                _ -> model
        in (m', CmdNone)

    Backspace ->
        let m' = case focus model of
                NumA -> let ns = if null (strA model) then "" else init (strA model)
                        in model { strA = ns, byteA = syncFromStr ns }
                NumB -> let ns = if null (strB model) then "" else init (strB model)
                        in model { strB = ns, byteB = syncFromStr ns }
                _ -> model
        in (m', CmdNone)

    ToggleMode ->
        let newMode = if mode model == UnsignedAdd then SignedAdd else UnsignedAdd
            m' = model { mode = newMode, strA = syncFromByte newMode (byteA model), strB = syncFromByte newMode (byteB model) }
        in (m', CmdNone)

--------------------------------------------------------------------------------
-- 4. SUBSCRIPTIONS
--------------------------------------------------------------------------------

handleSubs :: AppModel -> Sub AppAction
handleSubs _ = subBatch
    [ subKeyPress $ \key -> case key of
        KeyEscape    -> Just Quit
        KeyChar 'm'  -> Just ToggleMode
        KeyChar c | c `elem` ("0123456789-" :: String) -> Just (TypeChar c)
        KeyBackspace -> Just Backspace
        KeyLeft      -> Just (MoveCursor 1)
        KeyRight     -> Just (MoveCursor (-1))
        KeyUp        -> Just (AdjustValue 1)
        KeyDown      -> Just (AdjustValue (-1))
        _            -> Nothing
    , subEveryMs 100 AnimTick
    ]

--------------------------------------------------------------------------------
-- 5. VIEW
--------------------------------------------------------------------------------

renderBits :: Int -> [Bit] -> L
renderBits f bits = tightRow $ map draw (reverse bits)
  where
    pulse :: Double
    pulse = sin (fromIntegral f * 0.3)
    rVal  :: Int
    rVal  = round $ 177 + (78 * pulse)
    draw One  = tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                         , withStyle StyleBold $ withColor (ColorTrue rVal 0 0) (text "●")
                         , withColor (ColorTrue 60 60 60) (text "] ")
                         ]
    draw Zero = tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                         , withColor (ColorTrue 40 0 0) (text "●")
                         , withColor (ColorTrue 60 60 60) (text "] ")
                         ]

renderAluBits :: Int -> [Bit] -> L
renderAluBits f bits = tightRow $ map draw (reverse bits)
  where
    pulse :: Double
    pulse = sin (fromIntegral f * 0.3)
    gVal  :: Int
    gVal  = round $ 177 + (78 * pulse)
    draw One  = tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                         , withStyle StyleBold $ withColor (ColorTrue 0 gVal 0) (text "⬤")
                         , withColor (ColorTrue 60 60 60) (text "] ")
                         ]
    draw Zero = tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                         , withColor (ColorTrue 0 40 0) (text "⬤")
                         , withColor (ColorTrue 60 60 60) (text "] ")
                         ]

renderSwitches :: Focus -> String -> [Bit] -> L
renderSwitches foc target bits =
    let activeIdx = case foc of { SwA i | target == "A" -> i; SwB i | target == "B" -> i; _ -> -1 }
    in tightRow [ draw activeIdx i b | (i, b) <- zip [7,6..0] (reverse bits) ]
  where
    draw activeIdx i b =
        let isON = b == One
            char = if isON then "█" else "▄"
            col  = if isON then ColorTrue 0 255 255 else ColorTrue 60 60 60
            lBracket = if activeIdx == i then withStyle StyleBold $ withColor ColorBrightWhite (text " [") else withColor (ColorTrue 60 60 60) (text "  ")
            rBracket = if activeIdx == i then withStyle StyleBold $ withColor ColorBrightWhite (text "] ") else withColor (ColorTrue 60 60 60) (text "  ")
            swChar   = if activeIdx == i then withStyle StyleBold $ withColor ColorBrightWhite (text char) else withColor col (text char)
        in tightRow [lBracket, swChar, rBracket]

renderDigitalDisplay :: Bool -> String -> L
renderDigitalDisplay isFocused str =
    let displayStr = str ++ (if isFocused then "_" else "")
        padded = replicate (5 - length displayStr) ' ' ++ displayStr
        bracketL = withColor (ColorTrue 60 60 60) $ text "  [ "
        bracketR = withColor (ColorTrue 60 60 60) $ text " ]"
        content = if isFocused
                  then withStyle StyleBold $ withColor (ColorTrue 255 176 0) $ text padded
                  else withColor (ColorTrue 150 100 0) $ text padded
    in tightRow [ bracketL, content, bracketR ]

renderView :: AppModel -> L
renderView model = 
    let (Byte bitsA) = byteA model
        (Byte bitsB) = byteB model
        resByte = add (byteA model) (byteB model)
        (Byte resBits) = resByte
        isOV = checkOverflow (mode model) (byteA model) (byteB model)
        
        w = 62 
        centerTxt s = let p = max 0 (w - length s) `div` 2 in replicate p ' ' ++ s ++ replicate (w - length s - p) ' '
        
        thickDivider = withColor (ColorTrue 50 50 50) $ text (replicate w '▀')
        thinDivider  = withColor (ColorTrue 50 50 50) $ text (replicate w '─')

        renderHeader title =
            let titleSpacing = "  " ++ title ++ "  "
                leftBlocks = "███"
                rightLen = max 0 (w - length titleSpacing - length leftBlocks)
                rightBlocks = replicate rightLen '█'
            in tightRow
               [ withColor (ColorTrue 50 50 50) $ text leftBlocks
               , withStyle StyleBold $ withColor ColorBrightWhite $ text titleSpacing
               , withColor (ColorTrue 50 50 50) $ text rightBlocks
               ]

    in layout
    [ pad 1 $ withBorder BorderDouble $ box " VECTRONIX SYSTEMS : MAINFRAME 2000 "
        [ withStyle StyleBold $ withColor (ColorTrue 180 180 180) $ text $ centerTxt (show (mode model))
        , thickDivider
        
        , renderHeader "REGISTER A"
        , tightRow [ text "   ", renderBits (frame model) bitsA ]
        , tightRow [ text "   ", renderSwitches (focus model) "A" bitsA ]
        , renderDigitalDisplay (focus model == NumA) (strA model)
        
        , thinDivider
        
        , renderHeader "REGISTER B"
        , tightRow [ text "   ", renderBits (frame model) bitsB ]
        , tightRow [ text "   ", renderSwitches (focus model) "B" bitsB ]
        , renderDigitalDisplay (focus model == NumB) (strB model)
        
        , thickDivider
        
        , renderHeader "ALU ACCUMULATOR"
        , tightRow [ text "   ", renderAluBits (frame model) resBits ]
        , tightRow 
            [ renderDigitalDisplay False (show (getNumericValue (mode model) resByte))
            , text "      "
            , if isOV then withStyle StyleBold $ withColor (ColorTrue 255 0 0) (text "● OVF") else withColor (ColorTrue 40 0 0) (text "● OVF") 
            ]
        ]
    , withColor ColorBrightBlack $ text "  CONTROLS: [Left/Right] Focus | [Up/Down] Adjust | [m] Mode | [ESC] Quit"
    ]

--------------------------------------------------------------------------------
-- 6. MAIN APP RUNNER
--------------------------------------------------------------------------------

-- Safely queries the terminal size. Returns a default if it fails (e.g., in cabal run).
getInitialSize :: IO (Int, Int)
getInitialSize = do
    sz <- Term.size
    case sz of
        Just w  -> return (Term.width w, Term.height w)
        Nothing -> return (80, 24)

logicSimApp :: LayoutzApp AppModel AppAction
logicSimApp = LayoutzApp
    { appInit = (AppModel (int2byteSigned 0) (int2byteSigned 0) "0" "0" UnsignedAdd NumA 0, CmdNone)
    , appUpdate = handleAction, appSubscriptions = handleSubs, appView = renderView
    }

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    hSetBuffering stdin NoBuffering
    -- Use BlockBuffering to flush the entire frame at once, reducing tearing
    hSetBuffering stdout (BlockBuffering Nothing) 
    hSetEcho stdin False

     -- Check terminal size once at launch
    (w, _) <- getInitialSize

    if w < 64
        then do
            putStrLn $ "Error: Terminal is too narrow (" ++ show w ++ " columns)."
            putStrLn "The Logic Circuit Simulator requires at least 64 columns."
            putStrLn "Please zoom out or expand your window and restart."
            exitFailure
        else do
            putStr "\ESC[?25l"   -- Hide cursor
            putStr "\ESC[?1049h" -- Enter alternate screen buffer (isolates UI)
            putStr "\ESC[?7l"    -- Disable line wrapping (prevents vertical layout breakage)

            runApp logicSimApp

            putStr "\ESC[?7h"    -- Restore line wrapping
            putStr "\ESC[?1049l" -- Exit alternate screen buffer
            putStr "\ESC[?25h"   -- Restore cursor
