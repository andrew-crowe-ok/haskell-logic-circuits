{-# LANGUAGE OverloadedStrings #-}
module TUI 
    ( logicSimApp
    , runPostSequence
    , runShutdownSequence
    , getInitialSize
    ) where

import Layoutz
import Byte    ( Byte(..), byteToIntUnsigned, byteToIntSigned, int2byteSigned )
import Bit     ( Bit(..) )
import Classes ( Arithmetic(add) )
import Numeric (showHex, showOct)
import Control.Concurrent (threadDelay)
import qualified System.Console.Terminal.Size as Term (size, Window(width, height))


--------------------------------------------------------------------------------
-- 1. MODEL
--------------------------------------------------------------------------------

data AppMode = UnsignedAdd | SignedAdd deriving (Eq, Show, Enum, Bounded)

data DisplayBase = Dec | Hex | Oct deriving (Eq, Show, Enum, Bounded)

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
    , termW          :: Int
    , termH          :: Int
    , logLines       :: [String]
    , decayA         :: [Int]
    , decayB         :: [Int]
    , decayALU       :: [Int]
    , aluDisplayBits :: [Bit]
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

largeDigit :: Char -> [String]
largeDigit '0' = [" _ ", "| |", "|_|"]
largeDigit '1' = ["   ", "  |", "  |"]
largeDigit '2' = [" _ ", " _|", "|_ "]
largeDigit '3' = [" _ ", " _|", " _|"]
largeDigit '4' = ["   ", "|_|", "  |"]
largeDigit '5' = [" _ ", "|_ ", " _|"]
largeDigit '6' = [" _ ", "|_ ", "|_|"]
largeDigit '7' = [" _ ", "  |", "  |"]
largeDigit '8' = [" _ ", "|_|", "|_|"]
largeDigit '9' = [" _ ", "|_|", " _|"]
largeDigit '-' = ["   ", " _ ", "   "]
largeDigit '_' = ["   ", "   ", " _ "]
largeDigit 'x' = ["   ", "\\ /", "/ \\"]
largeDigit 'o' = ["   ", " _ ", "|_|"]
largeDigit 'a' = [" _ ", "|_|", "| |"]
largeDigit 'b' = ["   ", "|_ ", "|_|"]
largeDigit 'c' = [" _ ", "|  ", "|_ "]
largeDigit 'd' = ["   ", " _|", "|_|"]
largeDigit 'e' = [" _ ", "|_ ", "|_ "]
largeDigit 'f' = [" _ ", "|_ ", "|  "]
largeDigit ' ' = ["   ", "   ", "   "]
largeDigit _   = ["???", "???", "???"]

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

    Quit -> (model, CmdExit)

    AnimTick ->
        let newF = frame model + 1
            (Byte actualResBits) = add (byteA model) (byteB model)

            rippleRtoL target current = reverse $ step (reverse target) (reverse current)
              where
                step [] [] = []
                step (t:ts) (c:cs)
                  | t == c    = t : step ts cs
                  | otherwise = t : cs
                step _ _ = []

            newAluDisplay = rippleRtoL actualResBits (aluDisplayBits model)

            updateDecay (Byte bits) decays = zipWith (\b d -> if b == One then newF else d) bits decays
            newDecayA = updateDecay (byteA model) (decayA model)
            newDecayB = updateDecay (byteB model) (decayB model)
            newDecayALU = zipWith (\b d -> if b == One then newF else d) newAluDisplay (decayALU model)

        in (model { frame = newF, decayA = newDecayA, decayB = newDecayB, decayALU = newDecayALU, aluDisplayBits = newAluDisplay }, CmdNone)

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

    ToggleSwitch ->
        let toggle b = if b == One then Zero else One
            getBit idx (Byte bits) = bits !! idx
        in case focus model of
            SwA i -> let newByte = setBitAt i (toggle (getBit i (byteA model))) (byteA model)
                         m' = model { byteA = newByte, strA = syncFromByte (mode model) (displayBase model) newByte }
                     in (recordActivity m' ("> BUS INTERRUPT: REG A BIT " ++ show i ++ " TOGGLED"), CmdNone)
            SwB i -> let newByte = setBitAt i (toggle (getBit i (byteB model))) (byteB model)
                         m' = model { byteB = newByte, strB = syncFromByte (mode model) (displayBase model) newByte }
                     in (recordActivity m' ("> BUS INTERRUPT: REG B BIT " ++ show i ++ " TOGGLED"), CmdNone)
            _     -> (model, CmdNone)

    AdjustValue delta ->
        case focus model of
            NumA -> let current = getNumericValue (mode model) (byteA model)
                        newVal = current + delta
                        clamped = case mode model of
                            UnsignedAdd -> max 0 (min 255 newVal)
                            SignedAdd   -> max (-128) (min 127 newVal)
                        newByte = int2byteSigned clamped
                        m' = model { byteA = newByte, strA = syncFromByte (mode model) (displayBase model) newByte }
                    in (recordActivity m' "> DATA ENTRY: REG A VALUE SHIFTED", CmdNone)
            NumB -> let current = getNumericValue (mode model) (byteB model)
                        newVal = current + delta
                        clamped = case mode model of
                            UnsignedAdd -> max 0 (min 255 newVal)
                            SignedAdd   -> max (-128) (min 127 newVal)
                        newByte = int2byteSigned clamped
                        m' = model { byteB = newByte, strB = syncFromByte (mode model) (displayBase model) newByte }
                    in (recordActivity m' "> DATA ENTRY: REG B VALUE SHIFTED", CmdNone)
            _    -> (model, CmdNone)

    TypeChar c ->
        case focus model of
            NumA -> let ns = strA model ++ [c] 
                        m' = model { strA = ns, byteA = syncFromStr ns }
                    in (recordActivity m' "> BUS INTERRUPT: REGISTER STATE ALTERED", CmdNone)
            NumB -> let ns = strB model ++ [c] 
                        m' = model { strB = ns, byteB = syncFromStr ns }
                    in (recordActivity m' "> BUS INTERRUPT: REGISTER STATE ALTERED", CmdNone)
            _    -> (model, CmdNone)

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
        KeyChar 'q'  -> Just Quit
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

renderBits :: Int -> [Bit] -> [Int] -> L
renderBits currentF bits decays = tightRow $ map draw (reverse (zip bits decays))
  where
    pulse :: Double
    pulse = sin (fromIntegral currentF * 0.3)
    rVal  :: Int
    rVal  = round $ 177 + (78 * pulse)

    draw (One, _) = tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                             , withStyle StyleBold $ withColor (ColorTrue rVal 0 0) (text "●")
                             , withColor (ColorTrue 60 60 60) (text "] ")
                             ]
    draw (Zero, lastOnF) =
        let diff = currentF - lastOnF
            ghostCol = if diff <= 1 then ColorTrue 140 0 0
                       else if diff == 2 then ColorTrue 90 0 0
                       else ColorTrue 40 0 0
        in tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                    , withColor ghostCol (text "●")
                    , withColor (ColorTrue 60 60 60) (text "] ")
                    ]

renderAluBits :: Int -> [Bit] -> [Int] -> L
renderAluBits currentF bits decays = tightRow $ map draw (reverse (zip bits decays))
  where
    pulse :: Double
    pulse = sin (fromIntegral currentF * 0.3)
    gVal  :: Int
    gVal  = round $ 177 + (78 * pulse)

    draw (One, _) = tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                             , withStyle StyleBold $ withColor (ColorTrue 0 gVal 0) (text "⬤")
                             , withColor (ColorTrue 60 60 60) (text "] ")
                             ]
    draw (Zero, lastOnF) =
        let diff = currentF - lastOnF
            ghostCol = if diff <= 1 then ColorTrue 0 140 0
                       else if diff == 2 then ColorTrue 0 90 0
                       else ColorTrue 0 40 0
        in tightRow [ withColor (ColorTrue 60 60 60) (text " [")
                    , withColor ghostCol (text "⬤")
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
            col  = if isON then ColorTrue 180 180 180 else ColorTrue 60 60 60
            lBracket = if activeIdx == i then withStyle StyleBold $ withColor ColorBrightWhite (text " [") else withColor (ColorTrue 60 60 60) (text "  ")
            rBracket = if activeIdx == i then withStyle StyleBold $ withColor ColorBrightWhite (text "] ") else withColor (ColorTrue 60 60 60) (text "  ")
            swChar   = if activeIdx == i then withStyle StyleBold $ withColor ColorBrightWhite (text char) else withColor col (text char)
        in tightRow [lBracket, swChar, rBracket]

renderDigitalDisplay :: Int -> Bool -> String -> L -> [L]
renderDigitalDisplay currentF isFocused str appendRight =
    let safeStr = if null str then " " else str
        -- Blinks every ~500ms (5 frames)
        cursorChar = if isFocused && (currentF `mod` 10 < 5) then "_" else " "
        displayStr = safeStr ++ cursorChar
        
        padded = replicate (max 0 (5 - length displayStr)) ' ' ++ displayStr
        matrices = map largeDigit padded

        getRow i m = if length m > i then m !! i else "   "

        row1 = unwords (map (getRow 0) matrices)
        row2 = unwords (map (getRow 1) matrices)
        row3 = unwords (map (getRow 2) matrices)

        col = if isFocused then ColorTrue 255 176 0 else ColorTrue 150 100 0
        
        -- Increased left padding to 12 spaces for better centering under the switches
        formatRow r ext = tightRow [ text "                         ", withStyle StyleBold $ withColor col $ text r, text "   ", ext ]
    in [ formatRow row1 (text " ")
       , formatRow row2 appendRight
       , formatRow row3 (text " ")
       ]

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
    in tightRow [ text "    ", withColor (ColorTrue 0 150 150) $ text $
       "STATUS: " ++ targetInfo ++ " | " ++ baseInfo ]

renderSystemLights :: Int -> Int -> L
renderSystemLights currentF lastF =
    let isActive = currentF - lastF < 4 
        -- Fix GHC-18042: Explicit type for the pulse calculation
        pulse :: Double
        pulse = sin (fromIntegral currentF * 0.3)
        gVal  = round $ 177 + (78 * pulse)
        
        pwrCol = withStyle StyleBold $ withColor (ColorTrue 0 gVal 0) (text "● PWR")
        actCol = if isActive
                 then withStyle StyleBold $ withColor (ColorTrue 255 255 0) (text "● BUS")
                 else withColor (ColorTrue 60 60 0) (text "● BUS")
                 
        -- Hardware clock metronome ticking state
        clockStates = ["-", "\\", "|", "/"]
        clockChar = clockStates !! ((currentF `div` 2) `mod` 4)
        clkCol = withStyle StyleBold $ withColor (ColorTrue 0 150 150) (text $ "  CLK [" ++ clockChar ++ "]")
        
    in tightRow [ pwrCol, text "  ", actCol, clkCol ]

renderView :: AppModel -> L
renderView model =
    let (Byte bitsA) = byteA model
        (Byte bitsB) = byteB model
        isOV = checkOverflow (mode model) (byteA model) (byteB model)

        w = 72
        centerTxt s = let p = max 0 (w - length s) `div` 2 in replicate p ' ' ++ s ++ replicate (w - length s - p) ' '

        thickDivider = withColor (ColorTrue 50 50 50) $ text (replicate w '▓')
        
        busDivider label =
            let str = "[ " ++ label ++ " ]"
                remLen = max 0 (w - length str)
                leftChars = replicate (remLen `div` 2) '═'
                rightChars = replicate (remLen - (remLen `div` 2)) '═'
            in withColor (ColorTrue 80 80 80) $ text (leftChars ++ str ++ rightChars)

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

        ovfLight = if isOV then withStyle StyleBold $ withColor (ColorTrue 255 0 0) (text "● OVF") else withColor (ColorTrue 40 0 0) (text "● OVF")

    in layout
    [ pad 1 $ withBorder BorderDouble $ box " (+) ≡≡ VECTRONIX SYSTEMS : MAINFRAME 2000 ≡≡ (+) " $
        [ tightRow [ text "   ", renderSystemLights (frame model) (lastInputFrame model) ]
        , text $ centerTxt "MODE SELECTOR"
        , renderModeSwitch (mode model)
        
        , text " "
        , busDivider "I/O DATA BUS"

        , renderHeader "REGISTER A"
        , tightRow [ text "                ", renderBits (frame model) bitsA (decayA model) ]
        , tightRow [ text "                ", renderSwitches (focus model) "A" bitsA ]
        ] ++ renderDigitalDisplay (frame model) (focus model == NumA) (strA model) (text " ") ++
        [ thickDivider

        , renderHeader "REGISTER B"
        , tightRow [ text "                ", renderBits (frame model) bitsB (decayB model) ]
        , tightRow [ text "                ", renderSwitches (focus model) "B" bitsB ]
        ] ++ renderDigitalDisplay (frame model) (focus model == NumB) (strB model) (text " ") ++
        [ thickDivider

        , renderHeader "ALU ACCUMULATOR"
        , tightRow [ text "                ", renderAluBits (frame model) (aluDisplayBits model) (decayALU model) ]
        ] ++ renderDigitalDisplay (frame model) False (formatValue (displayBase model) (getNumericValue (mode model) (Byte (aluDisplayBits model)))) ovfLight ++
        [ text " "
        , busDivider "DIAGNOSTIC LOGIC BUS"

        , renderHeader "SYSTEM LOG"
        ] ++ 
        ( let linesToRender = if null (logLines model)
                              then ["> SYSTEM AWAITING INPUT..."]
                              else reverse (logLines model)
          in map (\s -> tightRow [ text "    ", withColor (ColorTrue 0 100 0) (text s) ]) linesToRender
        ) ++
        [ text " "
        , renderHeader "STATE MONITOR"
        , renderStatusBar (focus model) (displayBase model)
        ]
        
    -- FOOTER
    , withStyle StyleBold $ withColor ColorBrightWhite $ text "  [Tab] Next | [b] Prev | [Space] Toggle | [m] Mode | [h] Base | [q] Quit"
    ]

--------------------------------------------------------------------------------
-- 6. APP RUNNER HELPERS
--------------------------------------------------------------------------------

getInitialSize :: IO (Int, Int)
getInitialSize = do
    sz <- Term.size
    case sz of
        Just w  -> return (Term.width w, Term.height w)
        Nothing -> return (80, 24)

logicSimApp :: Int -> Int -> LayoutzApp AppModel AppAction
logicSimApp w h = LayoutzApp
    { appInit = (AppModel
        { byteA          = int2byteSigned 0
        , byteB          = int2byteSigned 0
        , strA           = "0"
        , strB           = "0"
        , mode           = UnsignedAdd
        , focus          = NumA
        , displayBase    = Dec
        , frame          = 0
        , lastInputFrame = 0
        , logLines       = []
        , decayA         = replicate 8 0
        , decayB         = replicate 8 0
        , decayALU       = replicate 8 0
        , aluDisplayBits = replicate 8 Zero
        , termW          = w
        , termH          = h
        }, CmdNone)
    , appUpdate = handleAction
    , appSubscriptions = handleSubs
    , appView = renderView
    }

runPostSequence :: IO ()
runPostSequence = do
    let delayMs :: Int -> IO ()
        delayMs ms = threadDelay (ms * 1000)
        
        typeStr :: String -> IO ()
        typeStr s = mapM_ (\c -> putStr [c] >> delayMs 15) s
        
        dotWait :: Int -> IO ()
        dotWait n = mapM_ (\_ -> putStr "." >> delayMs 200) [1..n]
    
    putStr "\ESC[32m" -- ANSI Green
    typeStr "VECTRONIX SYSTEMS BIOS v1.04\n"
    delayMs 500
    
    typeStr "0x0000 MEMORY CHECK "
    dotWait 8
    putStrLn " OK [640K]"
    delayMs 300
    
    typeStr "0x001A INITIALIZING I/O BUS "
    dotWait 5
    putStrLn " OK"
    delayMs 300
    
    typeStr "0x002F MOUNTING LOGIC PROCESSOR "
    dotWait 3
    putStrLn " OK"
    delayMs 500
    
    typeStr "SYSTEM READY. TRANSFERRING CONTROL TO FRONT PANEL"
    dotWait 5
    putStrLn "\n"
    putStr "\ESC[0m" -- ANSI Reset

runShutdownSequence :: IO ()
runShutdownSequence = do
    let delayMs :: Int -> IO ()
        delayMs ms = threadDelay (ms * 1000)
        
        typeStr :: String -> IO ()
        typeStr s = mapM_ (\c -> putStr [c] >> delayMs 15) s

    putStr "\ESC[2J\ESC[H\ESC[32m" -- Clear screen, cursor home, ANSI Green
    typeStr "INITIATING SYSTEM HALT...\n"
    delayMs 300
    typeStr "0x00F0 FLUSHING REGISTERS.......... OK\n"
    delayMs 300
    typeStr "0x00FA DISCONNECTING I/O BUS....... OK\n"
    delayMs 300
    typeStr "0x00FF POWER DOWN COMPLETE.\n"
    delayMs 800
    putStr "\ESC[0m" -- ANSI Reset
