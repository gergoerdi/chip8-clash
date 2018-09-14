{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module SDLIO.Video
    ( FrameBuf
    , withMainWindow
    ) where

import Clash.Prelude

import CHIP8.Types

import SDL hiding (get)
import Foreign.C.Types

import Linear (V4(..))
import Control.Monad (when, forM_)
import Data.Word
import Control.Monad.Trans (liftIO)

import Clash.Prelude (KnownNat)
import Clash.Sized.Unsigned
import Data.Array.IO
import Data.Array.MArray

type Color = V4 Word8

bgColor :: Color
bgColor = V4 90 160 110 maxBound

fgColor :: Color
fgColor = V4 0 0 0 maxBound

type FrameBuf = IOArray (VidX, VidY) Bit

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
    forM_ pixels $ \((x, y), b) -> when (b == high) $ do
        let x' = fromIntegral x * screenScale
            y' = fromIntegral y * screenScale
        fillRect renderer $ Just $ Rectangle (P $ V2 x' y') (V2 screenScale screenScale)

screenScale :: CInt
screenScale = 20

withMainWindow act = do
    initializeAll
    window <- createWindow "CHIP-8" defaultWindow
    windowSize window $= fmap (screenScale *) (V2 64 32)
    renderer <- createRenderer window (-1) defaultRenderer
    let render framebuf = liftIO $ do
            renderFrameBuf renderer framebuf
            present renderer
    act render
    destroyWindow window
