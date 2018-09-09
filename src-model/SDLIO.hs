{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Foldable (traverse_)

import Control.Monad.State hiding (state)
import Control.Monad.Writer
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
    = Exec
    | DrawCursor
    | Halt

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
    cpuOutVideo = RAMAddr (cursorX, cursorY) Nothing
    cpuOutMem = RAMAddr 0 Nothing

cpu :: CPUIn -> State CPUState CPUOut
cpu CPUIn{..} = (finish =<<) $ execWriterT $ do
    CPUState{..} <- get
    case state of
        Halt -> return ()
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
                    { cpuOutVideo = RAMAddr (cursorX, cursorY) (Just False)
                    }
                goto DrawCursor
        DrawCursor -> do
            tell $ Endo $ \out -> out
                { cpuOutVideo = RAMAddr (cursorX, cursorY) (Just True)
                }
            goto Exec
  where
    goto state = modify $ \s -> s{ state = state }

    finish f = gets $ appEndo f . defaultOut

main :: IO ()
main = withMainWindow $ \render -> do
    cpuState <- newIORef initialState

    framebuf <- newArray (minBound, maxBound) False
    ram <- newListArray @IOArray (minBound, maxBound) (prog ++ repeat 0)
    ramA <- newIORef Nothing

    let run key = do
            s <- readIORef cpuState
            a <- readIORef ramA
            cpuInMemRead <- maybe (return $ error "X") (readArray ram) a
            let cpuInKeyEvent = key
                cpuInVideoRead = False
                cpuIn = CPUIn{..}
            let (out@CPUOut{..}, s') = runState (cpu cpuIn) s
            writeIORef cpuState s'
            case cpuOutVideo of
                RAMAddr addr (Just val) -> writeArray framebuf addr val
                _ -> return ()
            case cpuOutMem of
                RAMAddr addr w -> do
                    writeIORef ramA $ Just addr
                    traverse_ (writeArray ram addr) w

    (`runContT` return) $ callCC $ \exit -> fix $ \loop -> do
    before <- ticks
    events <- pollEvents
    keyEvents <- fmap catMaybes $ forM events $ \event -> forM (userEvent $ eventPayload event) $ \ue -> case ue of
        Quit -> exit ()
        KeypadEvent pressed key -> return (pressed, key)

    liftIO $ do
      run Nothing
      mapM_ (run . Just) keyEvents

    render framebuf

    -- after <- ticks
    -- let elapsed = after - before
    -- when (elapsed < 20) $ do
    --     liftIO . threadDelay $ fromIntegral $ 20 - elapsed
    loop
