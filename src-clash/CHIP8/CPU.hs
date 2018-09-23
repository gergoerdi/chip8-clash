{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module CHIP8.CPU where

import Clash.Prelude

import CHIP8.Types
import CHIP8.Opcode
import CHIP8.ALU

import Cactus.Clash.Util
import Cactus.Clash.CPU
import Control.Monad.State
import Data.Word
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)

import Debug.Trace
import Text.Printf

data DrawPhase
    = DrawRead
    | DrawWrite

data Phase
    = Init
    | Fetch1
    | Exec
    | StoreReg Reg
    | LoadReg Reg
    | ClearFB (VidX, VidY)
    | Draw DrawPhase (VidX, VidY) Nybble (Index 8)
    | WaitKeyPress Reg
    | WriteBCD Word8 (Index 3)

succXY :: (Eq a, Bounded a, Enum a, Eq b, Bounded b, Enum b) => (a, b) -> Maybe (a, b)
succXY (x, y) =
    msum [ (x,) <$> succIdx y
         , (,minBound) <$> succIdx x
         ]

data CPUIn = CPUIn
    { cpuInMem :: Word8
    , cpuInFB :: Bit
    , cpuInKeys :: KeypadState
    , cpuInKeyEvent :: Maybe (Bool, Key)
    , cpuInVBlank :: Bool
    }

data CPUState = CPUState
    { opHi, opLo :: Word8
    , pc, ptr :: Addr
    , registers :: Vec 16 Word8
    , stack :: Vec 24 Addr
    , sp :: Index 24
    , phase :: Phase
    , timer :: Word8
    , randomState :: Unsigned 9
    }

initState :: CPUState
initState = CPUState
    { opHi = 0x00
    , opLo = 0x00
    , pc = 0x200
    , ptr = 0x000
    , registers = pure 0
    , stack = pure 0
    , sp = 0
    , phase = Init
    , timer = 0
    , randomState = 0
    }

data CPUOut = CPUOut
    { cpuOutMemAddr :: Addr
    , cpuOutMemWrite :: Maybe Word8
    , cpuOutFBAddr :: (VidX, VidY)
    , cpuOutFBWrite :: Maybe Bit
    }

defaultOut :: CPUState -> CPUOut
defaultOut CPUState{..} = CPUOut{..}
  where
    cpuOutMemAddr = pc
    cpuOutMemWrite = mzero
    cpuOutFBAddr = minBound
    cpuOutFBWrite = mzero

cpu :: CPU CPUIn CPUState CPUOut ()
cpu = do
    CPUIn{..} <- input
    CPUState{..} <- get
    modify $ \s -> s { randomState = lfsr randomState }
    when cpuInVBlank $ modify $ \s -> s{ timer = fromMaybe 0 $ predIdx timer }

    case phase of
        Init -> goto Fetch1
        Fetch1 -> do
            modify $ \s -> s
                { opHi = cpuInMem
                , pc = succ pc
                }
            goto Exec
        Exec -> do
            modify $ \s -> s
                { opLo = cpuInMem
                , pc = trace (printf "PC = %04x" (fromIntegral pc :: Word16)) $ succ pc
                }
            goto Fetch1
            exec
        StoreReg r -> case predIdx r of
            Nothing -> goto Fetch1
            Just r' -> storeReg r'
        LoadReg r -> loadReg r
        ClearFB xy -> clearFB xy
        Draw dp xy row col -> draw dp xy row col
        WaitKeyPress reg -> for_ cpuInKeyEvent $ \(pressed, key) -> when pressed $ do
            setReg reg $ fromIntegral key
            goto Fetch1
        WriteBCD x i -> case succIdx i of
            Nothing -> goto Fetch1
            Just i' -> do
                let addr = ptr + fromIntegral i'
                writeMem addr $ toBCDRom x !! i'
                goto $ WriteBCD x i'
  where
    goto ph = modify $ \s -> s{ phase = ph }

    clearFB xy = do
        writeFB xy low
        goto $ maybe Fetch1 ClearFB $ succXY xy

    setReg reg val = modify $ \s -> s{ registers = replace reg val (registers s) }
    getReg reg = gets $ (!! reg) . registers

    writeMem addr val = tell $ \out -> out{ cpuOutMemAddr = addr, cpuOutMemWrite = Just val }
    readMem addr = tell $ \out -> out{ cpuOutMemAddr = addr }

    writeFB xy val = tell $ \out -> out{ cpuOutFBAddr = xy, cpuOutFBWrite = Just val }
    readFB xy = tell $ \out -> out{ cpuOutFBAddr = xy, cpuOutFBWrite = Nothing }

    loadReg reg = do
        val <- cpuInMem <$> input
        setReg reg val
        case predIdx reg of
            Nothing -> goto Fetch1
            Just reg' -> do
                ptr <- gets ptr
                readMem (ptr + fromIntegral reg')
                goto $ LoadReg reg'

    storeReg reg = do
        ptr <- gets ptr
        val <- getReg reg
        writeMem (ptr + fromIntegral reg) val
        goto $ StoreReg reg

    popPC = modify $ \s@CPUState{..} -> let sp' = prevIdx sp in s
        { sp = sp'
        , pc = stack !! sp'
        }

    pushPC = modify $ \s@CPUState{..} -> s
        { sp = nextIdx sp
        , stack = replace sp pc stack
        }

    jump addr = modify $ \s -> s{ pc = addr }

    skip = do
        pc <- gets pc
        jump $ pc + 2

    exec = do
        CPUIn{..} <- input
        CPUState{opHi, opLo} <- get
        case traceShowId $ decode opHi opLo of
            ClearScreen -> clearFB minBound
            Ret -> do
                popPC
            -- Sys n -> do
            --     error $ unwords ["Unimplemented: SYS", show n]
            Jump addr -> do
                jump addr
            Call addr -> do
                pushPC
                jump addr
            SkipEqImm regX imm skipWhen -> do
                x <- getReg regX
                when ((x == imm) == skipWhen) skip
            SkipEqReg regX regY skipWhen -> do
                x <- getReg regX
                y <- getReg regY
                when ((x == y) == skipWhen) skip
            PutImm regX imm -> do
                setReg regX imm
            AddImm regX imm -> do
                x <- getReg regX
                setReg regX (x + imm)
            Move regX regY fun -> do
                x <- getReg regX
                y <- getReg regY
                let (x', carry) = alu fun x y
                setReg regX x'
                maybe (return ()) (setReg 0xf) carry
            SetPtr addr -> do
                modify $ \s -> s{ ptr = addr }
            JumpPlusR0 addr -> do
                x <- getReg 0
                jump (addr + fromIntegral x)
            Randomize regX mask -> do
                rnd <- gets $ fromIntegral . randomState
                setReg regX $ rnd .&. mask
            DrawSprite regX regY height -> do
                x <- fromIntegral <$> getReg regX
                y <- fromIntegral <$> getReg regY
                let lim = if height == 0 then 15 else height - 1
                setReg 0xf 0x00
                -- We draw from bottom to top, right to left. This
                -- allows using the remaining height/width being 0 as
                -- a stopping condition.
                draw DrawRead (x, y) lim maxBound
            SkipKey regX skipIfPressed -> do
                key <- fromIntegral <$> getReg regX
                let isPressed = cpuInKeys !! key
                when (isPressed == skipIfPressed) skip
            WaitKey regX -> goto $ WaitKeyPress regX
            GetTimer regX -> do
                setReg regX =<< gets timer
            SetTimer regX -> do
                val <- getReg regX
                modify $ \s -> s{ timer = val }
            -- SetSound regX -> do
            AddPtr regX -> do
                x <- getReg regX
                modify $ \s -> s{ ptr = ptr s + fromIntegral x }
            LoadFont regX -> do
                x <- getReg regX
                modify $ \s -> s{ ptr = toFont x }
            StoreBCD regX -> do
                x <- getReg regX
                ptr <- gets ptr
                writeMem ptr $ toBCDRom x !! 0
                goto $ WriteBCD x 0
            StoreRegs regMax -> storeReg regMax
            LoadRegs regMax -> do
                ptr <- gets ptr
                readMem (ptr + fromIntegral regMax)
                goto $ LoadReg regMax
            op -> errorX $ show op

    draw DrawWrite (x, y) row col = do
        CPUIn{..} <- input
        let b0 = cpuInFB
            b = cpuInMem ! (7 - col)
            b' = b0 `xor` b
        when (b0 .&. b == high) $ setReg 0x0f 0x01
        writeFB (x + fromIntegral col, y + fromIntegral row) $ b'
        let next = msum [ (row,) <$> predIdx col
                        , (,maxBound) <$> predIdx row
                        ]
        goto $ maybe Fetch1 (uncurry $ Draw DrawRead (x, y)) next
    draw DrawRead (x, y) row col = do
        ptr <- gets ptr
        readFB (x + fromIntegral col, y + fromIntegral row)
        readMem $ ptr + fromIntegral row
        goto $ Draw DrawWrite (x, y) row col

toBCDRom :: Word8 -> Vec 3 Word8
toBCDRom = asyncRom $(listToVecTH $ fmap toBCD [(minBound :: Word8)..maxBound])
