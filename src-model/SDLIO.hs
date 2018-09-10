{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, TypeApplications #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import SDLIO.Types
import SDLIO.Event
import SDLIO.Video
import SDLIO.Memory

import Cactus.Clash.CPU
import Control.Monad.State hiding (state)

import SDL hiding (get)
import Foreign.C.Types

import Control.Monad.Cont
import Data.IORef
import Data.Word
import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable (traverse_)

import Clash.Class.BitPack
import Clash.Sized.Index
import Clash.Sized.Unsigned
import Data.Array.IO
import Data.Array.MArray

type Addr = Unsigned 8

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

prog :: [RAMWord]
prog = map encode
    [ FlipPixel (10, 10)
    , FlipPixel (8, 10)
    , FlipPixel (6, 10)
    , FlipPixel (4, 10)
    , WaitKey
    , FlipPixel (10, 10)
    , FlipPixel (11, 11)
    , WaitKey
    , FlipPixel (11, 11)
    , WaitKey
    , End
    ]

data CPUIn = CPUIn
    { cpuInKeyEvent  :: Maybe (Bool, Index 16)
    , cpuInMemRead   :: RAMWord
    , cpuInVideoRead :: Bool
    }

data RAMAddr addr word = RAMAddr
    { ramAddr :: addr
    , ramWrite :: Maybe word
    }
    deriving (Show)

data CPUOut = CPUOut
    { cpuOutMem   :: RAMAddr Addr RAMWord
    , cpuOutVideo :: RAMAddr (VidX, VidY) Bool
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

    let goto state = modify $ \s -> s{ cpuState = state }
        fetch = do
            modify $ \s -> s{ cpuPC = succ $ cpuPC s }
            return $ decode cpuInMemRead

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
                            tell $ \out -> out
                                { cpuOutVideo = RAMAddr xy Nothing
                                }
                            goto WaitForVideo
                        End -> goto Halt
        WaitForVideo -> do
            case cpuIR of
                FlipPixel xy -> do
                    tell $ \out -> out
                        { cpuOutVideo = RAMAddr xy $ Just $ not cpuInVideoRead
                        }
                _ -> pure ()
            goto Exec

stateful :: (MonadIO m) => s -> (i -> State s o) -> IO (m i -> (o -> m a) -> m a)
stateful s0 step = do
    state <- newIORef s0
    return $ \mkInput applyOutput -> do
        inp <- mkInput
        out <- liftIO $ do
            s <- readIORef state
            let (out, s') = runState (step inp) s
            writeIORef state s'
            return out
        applyOutput out

main :: IO ()
main = withMainWindow $ \render -> do
    cpuState <- newIORef initialState

    framebuf <- mkMemory (minBound, maxBound) [] False
    ram <- mkMemory (minBound, maxBound) prog 0

    let mkInput key = do
            cpuInKeyEvent <- return key
            cpuInMemRead <- readData ram
            cpuInVideoRead <- readData framebuf
            return CPUIn{..}

        applyOutput CPUOut{..} = do
            case cpuOutVideo of
                RAMAddr addr val -> do
                    latchAddress framebuf addr
                    traverse_ (writeData framebuf addr) val
            case cpuOutMem of
                RAMAddr addr w -> do
                    latchAddress ram addr
                    traverse_ (writeData ram addr) w

    stepCPU <- stateful initialState cpu

    (`runContT` return) $ callCC $ \exit -> fix $ \loop -> do
    before <- ticks
    events <- pollEvents
    keyEvents <- fmap catMaybes $ forM events $ \event -> forM (userEvent $ eventPayload event) $ \ue -> case ue of
        Quit -> exit ()
        KeypadEvent pressed key -> return (pressed, key)

    let run key = stepCPU (mkInput key) applyOutput
    liftIO $ mapM_ run (Nothing : map Just keyEvents)

    render $ memBuf framebuf

    -- after <- ticks
    -- let elapsed = after - before
    -- when (elapsed < 20) $ do
    --     liftIO . threadDelay $ fromIntegral $ 20 - elapsed
    loop
