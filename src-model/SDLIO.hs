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

import SDL hiding (get)
import Foreign.C.Types

import Control.Monad.Cont
import Data.IORef
import Data.Word
import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable (traverse_)

import Control.Monad.State hiding (state)
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Cont
import Data.Monoid

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

newtype CPU i s o r a = CPU{ unCPU :: ContT r (RWS i (Endo o) s) a }
    deriving (Functor, Applicative, Monad, MonadState s)

output :: (o -> o) -> CPU i s o r ()
output = CPU . lift . tell . Endo

input :: CPU i s o r i
input = CPU $ lift ask

runCPU :: (s -> o) -> CPU i s o () () -> (i -> State s o)
runCPU mkDef cpu inp = do
    s <- get
    let (s', f) = execRWS (runContT (unCPU cpu) pure) inp s
    put s'
    def <- gets mkDef
    return $ appEndo f def

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
                            output $ \out -> out
                                { cpuOutVideo = RAMAddr xy Nothing
                                }
                            goto WaitForVideo
                        End -> goto Halt
        WaitForVideo -> do
            case cpuIR of
                FlipPixel xy -> do
                    output $ \out -> out
                        { cpuOutVideo = RAMAddr xy $ Just $ not cpuInVideoRead
                        }
                _ -> pure ()
            goto Exec

main :: IO ()
main = withMainWindow $ \render -> do
    cpuState <- newIORef initialState

    framebuf <- mkMemory (minBound, maxBound) [] False
    ram <- mkMemory (minBound, maxBound) prog 0

    let run key = do
            cpuInKeyEvent <- return key
            cpuInMemRead <- readData ram
            cpuInVideoRead <- readData framebuf
            let cpuIn = CPUIn{..}

            s <- readIORef cpuState
            let (CPUOut{..}, s') = runState (cpu cpuIn) s
            writeIORef cpuState s'

            case cpuOutVideo of
                RAMAddr addr val -> do
                    latchAddress framebuf addr
                    traverse_ (writeData framebuf addr) val
            case cpuOutMem of
                RAMAddr addr w -> do
                    latchAddress ram addr
                    traverse_ (writeData ram addr) w

    (`runContT` return) $ callCC $ \exit -> fix $ \loop -> do
    before <- ticks
    events <- pollEvents
    keyEvents <- fmap catMaybes $ forM events $ \event -> forM (userEvent $ eventPayload event) $ \ue -> case ue of
        Quit -> exit ()
        KeypadEvent pressed key -> return (pressed, key)

    liftIO $ do
      run Nothing
      mapM_ (run . Just) keyEvents

    render $ memBuf framebuf

    -- after <- ticks
    -- let elapsed = after - before
    -- when (elapsed < 20) $ do
    --     liftIO . threadDelay $ fromIntegral $ 20 - elapsed
    loop
