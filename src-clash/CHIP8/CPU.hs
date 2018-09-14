{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module CHIP8.CPU where

import Clash.Prelude

import CHIP8.Types
import CHIP8.Opcode

import Cactus.Clash.Util
import Cactus.Clash.CPU
import Control.Monad.State
import Data.Word

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

succXY :: (Eq a, Bounded a, Enum a, Eq b, Bounded b, Enum b) => (a, b) -> Maybe (a, b)
succXY (x, y) =
    msum [ (x,) <$> succIdx y
         , (,minBound) <$> succIdx x
         ]

data CPUState = CPUState
    { opHi, opLo :: Word8
    , pc, ptr :: Addr
    , registers :: Vec 16 Word8
    , stack :: Vec 24 Addr
    , sp :: Index 24
    , phase :: Phase
    }

data CPUIn = CPUIn
    { cpuInMem :: Word8
    , cpuInFB :: Bit
    , cpuInKeys :: KeypadState
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
    }

data CPUOut = CPUOut
    { cpuOutMemAddr :: Addr
    , cpuOutMemWrite :: Maybe Word8
    , cpuOutFBAddr :: (VidX, VidY)
    , cpuOutFBWrite :: Maybe Bit
    }

cpuOut :: CPUState -> CPUOut
cpuOut CPUState{..} = CPUOut{..}
  where
    cpuOutMemAddr = pc
    cpuOutMemWrite = mzero
    cpuOutFBAddr = minBound
    cpuOutFBWrite = mzero

cpu :: CPU CPUIn CPUState CPUOut ()
cpu = do
    CPUIn{..} <- input
    CPUState{..} <- get
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
                , pc = succ pc
                }
            goto Fetch1
            exec
        StoreReg r -> case predIdx r of
            Nothing -> goto Fetch1
            Just r' -> storeReg r'
        LoadReg r -> loadReg r
        ClearFB xy -> clearFB xy
        Draw dp xy row col -> draw dp xy row col
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
        case decode opHi opLo of
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
            -- Randomize regX mask -> do
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
            -- WaitKey regX -> do
            -- GetTimer regX -> do
            -- SetTimer regX -> do
            -- SetSound regX -> do
            AddPtr regX -> do
                x <- getReg regX
                modify $ \s -> s{ ptr = ptr s + fromIntegral x }
            LoadFont regX -> do
                x <- getReg regX
                modify $ \s -> s{ ptr = toFont x }
            -- StoreBCD regX -> do
            StoreRegs regMax -> storeReg regMax
            LoadRegs regMax -> do
                ptr <- gets ptr
                readMem (ptr + fromIntegral regMax)
                goto $ LoadReg regMax
            _ -> return ()

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
        readFB (x, y + fromIntegral row)
        readMem $ ptr + fromIntegral row
        goto $ Draw DrawWrite (x, y) row col

alu :: Fun -> Word8 -> Word8 -> (Word8, Maybe Word8)
alu fun = case fun of
    Id -> noCarry (\x y -> y)
    Or -> noCarry (.|.)
    And -> noCarry (.&.)
    XOr -> noCarry xor
    Add -> carry (+) (\x y z -> z < x)
    Subtract -> carry (-) (\x y z -> z <= x)
    SubtractFlip -> carry (flip (-)) (\x y z -> z <= y)
    ShiftRight -> carry (\x _ -> x `shiftR` 1) (\x _ _ -> x `testBit` 0)
    ShiftLeft -> carry (\x _ -> x `shiftL` 1) (\x _ _ -> x `testBit` 7)
  where
    noCarry f x y = (f x y, Nothing)
    carry f p x y = let z = f x y in (z, Just $ if p x y z then 1 else 0)

toBCD :: Word8 -> (Word8, Word8, Word8)
toBCD x = (x `div` 100, (x `div` 10) `mod` 10, x `mod` 10)

toFont :: Word8 -> Addr
toFont x = fromIntegral lo `shiftL` 3
  where
    (_, lo) = nybbles x
