{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Main where

import SDLIO.Types
import SDLIO.Event
import SDLIO.Video

import SDL hiding (get)
import Foreign.C.Types

import Control.Monad.Cont
import Data.IORef
import Data.Word
import Data.Maybe (catMaybes)

import Control.Monad.State hiding (state)
import Control.Monad.Writer
import Data.Monoid

import Clash.Sized.Index
import Data.Array.MArray

data CPUIn = CPUIn
    { cpuInKeyEvent :: Maybe (Bool, Index 16)
    }

data CPUOut = CPUOut
    { cpuOutVideoAddr :: (VidX, VidY)
    , cpuOutVideoWrite :: Maybe Bool
    }
    deriving (Show)

data Microstate
    = Exec
    | DrawCursor

data CPUState = CPUState
    { cursorX :: VidX
    , cursorY :: VidY
    , state :: Microstate
    }

initialState :: CPUState
initialState = CPUState
    { cursorX = 32
    , cursorY = 16
    , state = Exec
    }

defaultOut :: CPUState -> CPUOut
defaultOut CPUState{..} = CPUOut{..}
  where
    cpuOutVideoAddr = (cursorX, cursorY)
    cpuOutVideoWrite = Nothing

cpu :: CPUIn -> State CPUState CPUOut
cpu CPUIn{..} = (finish =<<) $ execWriterT $ do
    CPUState{..} <- get
    case state of
        Exec -> do
            let dxy = case cpuInKeyEvent of
                    Just (True, 0x07) -> Just (-1, 0)
                    Just (True, 0x09) -> Just (1, 0)
                    Just (True, 0x05) -> Just (0, -1)
                    Just (True, 0x08) -> Just (0, 1)
                    _ -> Nothing
            forM_ dxy $ \(dx, dy) -> do
                let x' = cursorX + dx
                    y' = cursorY + dy
                modify $ \s -> s{ cursorX = x', cursorY = y' }
                tell $ Endo $ \out -> out
                    { cpuOutVideoAddr = (cursorX, cursorY)
                    , cpuOutVideoWrite = Just False
                    }
                goto DrawCursor
        DrawCursor -> do
            tell $ Endo $ \out -> out
                { cpuOutVideoWrite = Just True
                }
            goto Exec
  where
    goto state = modify $ \s -> s{ state = state }

    finish f = gets $ appEndo f . defaultOut

main :: IO ()
main = withMainWindow $ \render -> do
    cpuState <- newIORef initialState

    framebuf <- newArray (minBound, maxBound) False

    let run key = do
            s <- liftIO $ readIORef cpuState
            let cpuInKeyEvent = key
                cpuIn = CPUIn{..}
            let (out@CPUOut{..}, s') = runState (cpu cpuIn) s
            liftIO $ writeIORef cpuState s'
            liftIO $ mapM_ (writeArray framebuf cpuOutVideoAddr) cpuOutVideoWrite

    (`runContT` return) $ callCC $ \exit -> fix $ \loop -> do
    before <- ticks
    events <- pollEvents
    keyEvents <- fmap catMaybes $ forM events $ \event -> forM (userEvent $ eventPayload event) $ \ue -> case ue of
        Quit -> exit ()
        KeypadEvent pressed key -> return (pressed, key)

    run Nothing
    mapM_ (run . Just) keyEvents

    render framebuf

    -- after <- ticks
    -- let elapsed = after - before
    -- when (elapsed < 20) $ do
    --     liftIO . threadDelay $ fromIntegral $ 20 - elapsed
    loop
