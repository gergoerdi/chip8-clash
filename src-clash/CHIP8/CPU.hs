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

data Phase
    = Init
    | Fetch1
    | Exec
    | StoreReg Reg
    | LoadReg Reg
    | ClearFB (VidX, VidY)

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
    , cpuInFB :: Bool
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
    , cpuOutFBWrite :: Maybe Bool
    }

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
  where
    goto ph = modify $ \s -> s{ phase = ph }

    clearFB xy = do
        writeFB xy False
        goto $ maybe Fetch1 ClearFB $ succXY xy

    setReg reg val = modify $ \s -> s{ registers = replace reg val (registers s) }
    getReg reg = gets $ (!! reg) . registers

    writeMem addr val = tell $ \out -> out{ cpuOutMemAddr = addr, cpuOutMemWrite = Just val }
    readMem addr = tell $ \out -> out{ cpuOutMemAddr = addr }

    writeFB xy val = tell $ \out -> out{ cpuOutFBAddr = xy, cpuOutFBWrite = Just val }

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

    popAddr = modify $ \s@CPUState{..} -> let sp' = prevIdx sp in s
        { sp = sp'
        , ptr = stack !! sp'
        }

    pushAddr = modify $ \s@CPUState{..} -> s
        { sp = nextIdx sp
        , stack = replace sp ptr stack
        }

    jump addr = modify $ \s -> s{ pc = addr }

    skip = do
        ptr <- gets ptr
        jump $ ptr + 2

    exec = do
        CPUIn{..} <- input
        CPUState{opHi, opLo} <- get
        case decode opHi opLo of
            ClearScreen -> clearFB minBound
            Ret -> do
                popAddr
            -- Sys n -> do
            --     error $ unwords ["Unimplemented: SYS", show n]
            Jump addr -> do
                jump addr
            Call addr -> do
                pushAddr
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
            -- DrawSprite regX regY height -> do
            -- SkipKey regX skipIfPressed -> do
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
