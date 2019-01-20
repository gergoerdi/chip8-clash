{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Clash.Prelude

import CHIP8.Types
import CHIP8.CPU
import CHIP8.Font
import Cactus.Clash.CPU

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
import qualified Data.List as L
import qualified Data.ByteString as BS

import Text.Printf
import System.Environment (getArgs)

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
main = do
    imgFile <- do
        args <- getArgs
        return $ case args of
            [imgFile] -> imgFile
            _ -> "games/hidden.ch8"
    prog <- BS.unpack <$> BS.readFile imgFile

    framebuf <- mkMemory (minBound, maxBound) [] low
    ram <- mkMemory (minBound, maxBound) (L.replicate 0x200 0 <> prog) 0
    fontROM <- mkMemory (0, 127) (toList hexDigits) 0
    keys <- newIORef $ pure False
    memAddr <- newIORef 0x00

    withMainWindow $ \render -> do
    let mkInput vblank key = do
            liftIO $ modifyIORef keys $ maybe id applyKeyEvent key
            cpuInKeys <- readIORef keys
            cpuInMem <- do
                addr <- liftIO $ readIORef memAddr
                readData $ if addr < 128 then fontROM else ram
            cpuInFB <- readData framebuf
            let cpuInKeyEvent = key
                cpuInVBlank = vblank
            return CPUIn{..}

        applyOutput CPUOut{..} = do
            latchAddress framebuf cpuOutFBAddr
            traverse_ (writeData framebuf cpuOutFBAddr) cpuOutFBWrite

            -- liftIO $ printf "0x%04x\n" (fromIntegral cpuOutMemAddr :: Int)

            liftIO $ writeIORef memAddr cpuOutMemAddr
            latchAddress fontROM cpuOutMemAddr
            latchAddress ram cpuOutMemAddr
            traverse_ (writeData ram cpuOutMemAddr) cpuOutMemWrite

    stepCPU <- stateful initState $ runCPU defaultOut cpu

    (`runContT` return) $ callCC $ \exit -> fix $ \loop -> do
    before <- ticks
    events <- pollEvents
    keyEvents <- fmap catMaybes $ forM events $ \event -> forM (userEvent $ eventPayload event) $ \ue -> case ue of
        Quit -> exit ()
        KeypadEvent pressed key -> return (pressed, key)

    let run vblank key = stepCPU (mkInput vblank key) applyOutput
    liftIO $ run True Nothing
    liftIO $ mapM_ (run False . Just) keyEvents

    render $ memBuf framebuf

    fix $ \inner -> do
        after <- ticks
        let elapsed = after - before
        when (elapsed < 20) $ do
            liftIO $ run False Nothing
            inner
    loop

applyKeyEvent :: (Bool, Key) -> KeypadState -> KeypadState
applyKeyEvent (pressed, key) = replace key pressed
