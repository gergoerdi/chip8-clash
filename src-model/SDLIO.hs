{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import SDLIO.Types
import SDLIO.CPU
import SDLIO.Event
import SDLIO.Video
import SDLIO.Memory

import SDL hiding (get)
import Foreign.C.Types
import Control.Monad.State hiding (state)
import Control.Monad.Cont
import Data.IORef
import Data.Maybe (catMaybes, fromMaybe)
import Data.Foldable (traverse_)

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
