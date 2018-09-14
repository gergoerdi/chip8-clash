{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module SDLIO.CPU where

import Clash.Prelude

import CHIP8.Types

import Cactus.Clash.CPU
import Control.Monad.State hiding (state)
import Clash.Class.BitPack
import Clash.Sized.Index
import Clash.Sized.Unsigned

data Opcode
    = FlipPixel (VidX, VidY)
    | WaitKey
    | End
    deriving (Show)

type RAMWord = Unsigned 12

encode :: Opcode -> RAMWord
encode End = unpack . pack $ (0 :: Unsigned 1, 0 :: Unsigned 11)
encode WaitKey = unpack . pack $ (0 :: Unsigned 1, 1 :: Unsigned 11)
encode (FlipPixel (x, y)) = unpack . pack $ (1 :: Unsigned 1, x, y)

decode :: RAMWord -> Maybe Opcode
decode w = case (op, x, y) of
    (0, 0, 1) -> Just WaitKey
    (0, 0, 0) -> Just End
    (1, x, y) -> Just $ FlipPixel (x, y)
    _ -> Nothing
  where
    op :: Unsigned 1
    (op, x, y) = unpack . pack $ w

data CPUIn = CPUIn
    { cpuInKeyEvent  :: Maybe (Bool, Index 16)
    , cpuInMemRead   :: RAMWord
    , cpuInVideoRead :: Bit
    }

data RAMAddr addr word = RAMAddr
    { ramAddr :: addr
    , ramWrite :: Maybe word
    }
    deriving (Show)

data CPUOut = CPUOut
    { cpuOutMem   :: RAMAddr Addr RAMWord
    , cpuOutVideo :: RAMAddr (VidX, VidY) Bit
    }
    deriving (Show)

data Microstate
    = Init
    | Exec
    | WaitForVideo
    | WaitForKey
    | Halt

data CPUState = CPUState
    { cpuPC :: Addr
    , cpuIR :: Opcode
    , cpuState :: Microstate
    }

initialState :: CPUState
initialState = CPUState
    { cpuPC = 0
    , cpuIR = End
    , cpuState = Init
    }

defaultOut :: CPUState -> CPUOut
defaultOut CPUState{..} = CPUOut{..}
  where
    cpuOutVideo = RAMAddr minBound Nothing
    cpuOutMem = RAMAddr cpuPC Nothing

cpu :: CPUIn -> State CPUState CPUOut
cpu = runCPU defaultOut $ do
    CPUIn{..} <- input
    CPUState{..} <- get

    case cpuState of
        Init -> goto Exec
        Halt -> return ()
        WaitForKey -> do
            case cpuInKeyEvent of
                Just (True, key) -> goto Exec
                _ -> return ()
        Exec -> do
            op <- fetch
            case op of
                Nothing -> do
                    goto Halt
                Just op -> do
                    modify $ \s -> s{ cpuIR = op }
                    case op of
                        WaitKey -> goto WaitForKey
                        FlipPixel xy -> do
                            tellVideo xy Nothing
                            goto WaitForVideo
                        End -> goto Halt
        WaitForVideo -> do
            case cpuIR of
                FlipPixel xy -> tellVideo xy . Just $ complement cpuInVideoRead
                _ -> pure ()
            goto Exec
  where
    goto state = modify $ \s -> s{ cpuState = state }

    tellVideo xy b = tell $ \out -> out{ cpuOutVideo = RAMAddr xy b }

    fetch = do
        modify $ \s -> s{ cpuPC = succ $ cpuPC s }
        CPUIn{..} <- input
        return $ decode cpuInMemRead
