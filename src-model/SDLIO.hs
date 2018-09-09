{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Main where

import SDLIO.Event

import SDL hiding (get)
import Foreign.C.Types

import Data.Function (fix)
import Linear (V4(..))
import Control.Monad (unless)
import Control.Monad.Cont
import Data.Traversable
import Data.IORef
import Data.Word
import Control.Concurrent (threadDelay)
import Data.Maybe (catMaybes)

import Control.Monad.State hiding (state)
import Control.Monad.Writer
import Data.Monoid

import Clash.Prelude (KnownNat)
import Clash.Sized.Vector as V
import Clash.Sized.Index
import Clash.Sized.Unsigned
import Data.Array.IO
import Data.Array.MArray

type Color = V4 Word8

bgColor :: Color
bgColor = V4 90 160 110 maxBound

fgColor :: Color
fgColor = V4 0 0 0 maxBound

type VidX = Unsigned 6
type VidY = Unsigned 5

type FrameBuf = IOArray (VidX, VidY) Bool

instance (KnownNat n) => Ix (Unsigned n) where
    range (lo, hi) = [lo..hi]
    index (lo, hi) x = fromIntegral $ x - lo
    inRange (lo, hi) x = lo <= x && x <= hi

renderFrameBuf :: Renderer -> FrameBuf -> IO ()
renderFrameBuf renderer fb = do
    rendererDrawColor renderer $= bgColor
    clear renderer

    rendererDrawColor renderer $= fgColor
    pixels <- getAssocs fb
    forM_ pixels $ \((x, y), b) -> when b $ do
        let x' = fromIntegral x * size
            y' = fromIntegral y * size
        fillRect renderer $ Just $ Rectangle (P $ V2 x' y') (V2 size size)

size :: CInt
size = 20

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
main = do
    initializeAll
    window <- createWindow "CHIP-8" defaultWindow
    windowSize window $= fmap (size *) (V2 64 32)
    renderer <- createRenderer window (-1) defaultRenderer

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

        liftIO $ renderFrameBuf renderer framebuf
        present renderer

        -- after <- ticks
        -- let elapsed = after - before
        -- when (elapsed < 20) $ do
        --     liftIO . threadDelay $ fromIntegral $ 20 - elapsed
        loop
    destroyWindow window
