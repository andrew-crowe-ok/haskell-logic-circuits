{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Layoutz
import Byte    ( Byte(..), byteToIntUnsigned, byteToIntSigned, int2byteSigned )
import Bit     ( Bit(..) )
import Classes ( Arithmetic(add) )
import System.IO (hSetEncoding, hSetBuffering, hSetEcho, stdout, stdin, utf8, BufferMode(..))
import System.Exit (exitFailure)
import Numeric (showHex, showOct)
import qualified System.Console.Terminal.Size as Term (size, Window(width, height))

--------------------------------------------------------------------------------
-- 1. MODEL
--------------------------------------------------------------------------------

data AppMode = UnsignedAdd | SignedAdd deriving (Eq, Enum, Bounded)

data DisplayBase = Dec | Hex | Oct deriving (Eq, Show, Enum, Bounded)

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
    , displayBase    :: DisplayBase
    , frame          :: Int
    , lastInputFrame :: Int
    , logLines       :: [String]
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

syncFromByte :: AppMode -> DisplayBase -> Byte -> String
syncFromByte m b base = formatValue b (getNumericValue m base)

setBitAt :: Int -> Bit -> Byte -> Byte
setBitAt idx newBit (Byte bits) =
    case splitAt idx bits of
        (pre, _:post) -> Byte (pre ++ newBit : post)
        _             -> Byte bits

formatValue :: DisplayBase -> Int -> String
formatValue Dec n = show n
formatValue Hex n = "0x" ++ showHex (if n < 0 then n + 256 else n) ""
formatValue Oct n = "0o" ++ showOct (if n < 0 then n + 256 else n) ""


--------------------------------------------------------------------------------
-- 2. ACTIONS
--------------------------------------------------------------------------------

data AppAction
    = TypeChar Char
    | Backspace
    | CycleFocusForward
    | CycleFocusBackward
    | AdjustValue Int
    | ToggleSwitch
    | ToggleMode
    | ToggleBase
    | AnimTick
    | Quit

--------------------------------------------------------------------------------
-- 3. UPDATE
--------------------------------------------------------------------------------

handleAction :: AppAction -> AppModel -> (AppModel, Cmd AppAction)
handleAction action model = case action of
    Quit             -> (model, CmdExit)
    AnimTick         -> (model { frame = frame model + 1 }, CmdNone)
    CycleFocusForward ->
        let nextFocus = case focus model of
                SwA 0 -> NumA
                SwA i -> SwA (i - 1)
                NumA  -> SwB 7
                SwB 0 -> NumB
                SwB i -> SwB (i - 1)
                NumB  -> SwA 7
        in (model { focus = nextFocus }, CmdNone)
    CycleFocusBackward ->
        let prevFocus = case focus model of
                NumA  -> SwA 0
                SwA 7 -> NumB
                SwA i -> SwA (i + 1)
                NumB  -> SwB 0
                SwB 7 -> NumA
                SwB i -> SwB (i + 1)
        in (model { focus = prevFocus }, CmdNone)
    ToggleSwitch ->
        let toggle b = if b == One then Zero else One
            getBit idx (Byte bits) = bits !! idx 
            m' = case focus model of
                    SwA i -> let newByte = setBitAt i (toggle (getBit i (byteA model))) (byteA model)
                             in model { byteA = newByte, strA = syncFromByte (mode model) (displayBase model) newByte }
                    SwB i -> let newByte = setBitAt i (toggle (getBit i (byteB model))) (byteB model)
                             in model { byteB = newByte, strB = syncFromByte (mode model) (displayBase model) newByte }
                    _     -> model
            logMsg = case focus model of
                SwA i -> "> BUS INTERRUPT: REG A BIT " ++ show i ++ " TOGGLED"
                SwB i -> "> BUS INTERRUPT: REG B BIT " ++ show i ++ " TOGGLED"
                _     -> "> BUS INTERRUPT: SWITCH TOGGLED"
        in (recordActivity m' logMsg, CmdNone)

    ToggleMode ->
        let newMode = if mode model == UnsignedAdd then SignedAdd else UnsignedAdd
            m' = model { mode = newMode
                       , strA = syncFromByte newMode (displayBase model) (byteA model)
                       , strB = syncFromByte newMode (displayBase model) (byteB model) 
                       }
            logMsg = "> SYS CTRL: ARITHMETIC MODE SET TO " ++ show newMode
        in (recordActivity m' logMsg, CmdNone)
    ToggleBase ->
        let nextBase = if displayBase model == Oct then Dec else succ (displayBase model)
            m' = model { displayBase = nextBase
                       , strA = syncFromByte (mode model) nextBase (byteA model)
                       , strB = syncFromByte (mode model) nextBase (byteB model)
                       }
        in (m', CmdNone)
    AdjustValue delta ->
        let m' = case focus model of
                NumA -> let current = getNumericValue (mode model) (byteA model)
                            newVal = current + delta
                            clamped = case mode model of
                                UnsignedAdd -> max 0 (min 255 newVal)
                                SignedAdd   -> max (-128) (min 127 newVal)
                            newByte = int2byteSigned clamped
                        in model { byteA = newByte, strA = syncFromByte (mode model) (displayBase model) newByte }
                NumB -> let current = getNumericValue (mode model) (byteB model)
                            newVal = current + delta
                            clamped = case mode model of
                                UnsignedAdd -> max 0 (min 255 newVal)
                                SignedAdd   -> max (-128) (min 127 newVal)
                            newByte = int2byteSigned clamped
                        in model { byteB = newByte, strB = syncFromByte (mode model) (displayBase model) newByte }
                _ -> model
            logMsg = case focus model of
                 NumA -> "> DATA ENTRY: REG A VALUE SHIFTED"
                 NumB -> "> DATA ENTRY: REG B VALUE SHIFTED"
                 _    -> "> DATA ENTRY: VALUE SHIFTED"
        in (recordActivity m' logMsg, CmdNone)
    TypeChar c ->
        let m' = case focus model of
                NumA -> let ns = strA model ++ [c] in model { strA = ns, byteA = syncFromStr ns }
                NumB -> let ns = strB model ++ [c] in model { strB = ns, byteB = syncFromStr ns }
                _ -> model
            logMsg = "> BUS INTERRUPT: REGISTER STATE ALTERED"
        in (recordActivity m' logMsg, CmdNone)
    Backspace ->
        let m' = case focus model of
                NumA -> let ns = if null (strA model) then "" else init (strA model)
                        in model { strA = ns, byteA = syncFromStr ns }
                NumB -> let ns = if null (strB model) then "" else init (strB model)
                        in model { strB = ns, byteB = syncFromStr ns }
                _ -> model
        in (m', CmdNone)

recordActivity :: AppModel -> String -> AppModel
recordActivity m msg = 
    let newLog = take 3 (msg : logLines m)
    in m { lastInputFrame = frame m, logLines = newLog }

--------------------------------------------------------------------------------
-- 4. SUBSCRIPTIONS
--------------------------------------------------------------------------------

handleSubs :: AppModel -> Sub AppAction
handleSubs _ = subBatch
    [ subKeyPress $ \key -> case key of
        KeyChar 'm'  -> Just ToggleMode
        KeyChar ' '  -> Just ToggleSwitch
        KeyEscape    -> Just Quit
        KeyTab       -> Just CycleFocusForward
        KeyChar 'b'  -> Just CycleFocusBackward
        KeyChar 'h'  -> Just ToggleBase
        KeyBackspace -> Just Backspace
        KeyUp        -> Just (AdjustValue 1)
        KeyDown      -> Just (AdjustValue (-1))
        KeyChar c | c `elem` ("0123456789-" :: String) -> Just (TypeChar c)
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

renderModeSwitch :: AppMode -> L
renderModeSwitch m = 
    let labelU = if m == UnsignedAdd then "[ UNSIGNED ]" else "  UNSIGNED  "
        labelS = if m == SignedAdd   then "[  SIGNED  ]" else "   SIGNED   "
        lever  = if m == UnsignedAdd then "  (O)──┤  " else "  ├──(O)  "
        
        fullStrLen = length labelU + length lever + length labelS
        padLen = max 0 (62 - fullStrLen) `div` 2
        padding = text (replicate padLen ' ')
        
        lU = if m == UnsignedAdd then withStyle StyleBold (withColor ColorBrightWhite (text labelU)) else withColor (ColorTrue 60 60 60) (text labelU)
        lS = if m == SignedAdd   then withStyle StyleBold (withColor ColorBrightWhite (text labelS)) else withColor (ColorTrue 60 60 60) (text labelS)
        lLev = withColor ColorBrightWhite (text lever)
    in tightRow [ padding, lU, lLev, lS ]

renderStatusBar :: Focus -> DisplayBase -> L
renderStatusBar foc base =
    let targetInfo = case foc of
            NumA   -> "REG A [ NUMERIC ENTRY ]"
            SwA i  -> "REG A [ SWITCH BIT " ++ show i ++ " ]"
            NumB   -> "REG B [ NUMERIC ENTRY ]"
            SwB i  -> "REG B [ SWITCH BIT " ++ show i ++ " ]"
        baseInfo = "BASE [ " ++ show base ++ " ]"
    in withColor (ColorTrue 0 150 150) $ text $ 
       "  STATUS: " ++ targetInfo ++ " | " ++ baseInfo

renderSystemLights :: Int -> Int -> L
renderSystemLights currentF lastF = 
    let isActive = currentF - lastF < 4 -- Flash for ~400ms
        pwrCol = withColor (ColorTrue 0 200 0) (text "● PWR")
        actCol = if isActive 
                 then withStyle StyleBold $ withColor (ColorTrue 255 255 0) (text "● BUS") 
                 else withColor (ColorTrue 60 60 0) (text "● BUS")
    in tightRow [ pwrCol, text "  ", actCol ]

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
        [ tightRow [ text "   ", renderSystemLights (frame model) (lastInputFrame model) ]
        , text $ centerTxt "MODE SELECTOR"
        , renderModeSwitch (mode model)
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
            [ renderDigitalDisplay False (formatValue (displayBase model) (getNumericValue (mode model) resByte))
            , text "      "
            , if isOV then withStyle StyleBold $ withColor (ColorTrue 255 0 0) (text "● OVF") else withColor (ColorTrue 40 0 0) (text "● OVF") 
            ]
        
        , thickDivider
        
        , renderHeader "SYSTEM LOG"
        , withColor (ColorTrue 0 100 0) $ box "" (map text (reverse $ logLines model))
        
        -- Relocated Status Bar inside the hardware chassis
        , renderStatusBar (focus model) (displayBase model)
        ]
    , withColor ColorBrightBlack $ text "  [Tab] Next | [b] Prev | [Space] Toggle | [m] Mode | [h] Base | [ESC] Quit"
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
    { appInit = (AppModel (int2byteSigned 0) (int2byteSigned 0) "0" "0" UnsignedAdd NumA Dec 0 0 [], CmdNone)
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
