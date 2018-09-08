{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}
module CHIP8 where

import Clash.Prelude hiding (clkPeriod)
import Cactus.Clash.Util
import Cactus.Clash.SerialTX
import Cactus.Clash.SerialRX
import Cactus.Clash.VGA
import Cactus.Clash.PS2
import Data.Word
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad (guard)
import Data.Function
import Data.Proxy

-- 25.175 MHz clock, needed for the VGA mode we use
type Dom25 = Dom "CLK_25MHZ" (1000000000000 `Div` 25175000)

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "CHIP8"
    , t_inputs =
          [ PortName "CLK_25MHZ"
          , PortName "RESET"
          , PortName "RX"
          , PortName "PS2_CLK"
          , PortName "PS2_DATA"
          ]
    , t_output = PortProduct ""
          [ PortName "TX"
          , PortProduct "" [ PortName "VGA_VSYNC", PortName "VGA_HSYNC", PortName "VGA_RED", PortName "VGA_GREEN", PortName "VGA_BLUE" ]
          ]
    }) #-}
topEntity
    :: Clock Dom25 Source
    -> Reset Dom25 Asynchronous
    -> Signal Dom25 Bit
    -> Signal Dom25 Bit
    -> Signal Dom25 Bit
    -> ( Signal Dom25 Bit
      , ( Signal Dom25 Bit
        , Signal Dom25 Bit
        , Signal Dom25 (Unsigned 4)
        , Signal Dom25 (Unsigned 4)
        , Signal Dom25 (Unsigned 4)
        )
      )
topEntity = exposeClockReset board
  where
    board rxIn ps2Clk ps2Data = (txOut, (vgaVSync, vgaHSync, vgaR, vgaG, vgaB))
      where
        VGADriver{..} = vgaDriver vga640x480at60

        vgaX' = (chipX =<<) <$> vgaX
        vgaY' = (chipY =<<) <$> vgaY
        visible = isJust <$> vgaX' .&&. isJust <$> vgaY'

        ps2 = decodePS2 $ samplePS2 PS2{..}

        txOut = pure low

        (dx, dy) = unbundle $ do
            key <- parseScanCode ps2
            pure $ case key of
                Just (ScanCode KeyPress 0xe075) -> (0, -1) -- up
                Just (ScanCode KeyPress 0xe072) -> (0, 1)  -- down
                Just (ScanCode KeyPress 0xe06b) -> (-1, 0) -- left
                Just (ScanCode KeyPress 0xe074) -> (1, 0)  -- right
                _ -> (0, 0)

        x0 = register 0 (x0 + dx)
        y0 = register 0 (y0 + dy)

        pixel = pixelAt <$> x0 <*> y0 <*> vgaX' <*> vgaY'
          where
            pixelAt x0 y0 mx my = case (,) <$> mx <*> my of
                Just (x, y) -> x == x0 && y == y0
                _ -> False

        vgaR = monochrome <$> pixel
        vgaG = monochrome <$> pixel
        vgaB = monochrome <$> pixel

monochrome :: (Bounded a) => Bool -> a
monochrome b = if b then maxBound else minBound

chipX :: Unsigned 10 -> Maybe (Unsigned 6)
chipX x = let (x', _) = unpack . pack $ x :: (Unsigned 7, Unsigned 3)
          in enable (8 <= x' && x' < 8 + 64) (truncateB $ x' - 8)

chipY :: Unsigned 10 -> Maybe (Unsigned 5)
chipY y = let (y', _) = unpack . pack $ y :: (Unsigned 7, Unsigned 3)
          in enable (14 <= y' && y' < 14 + 32) (truncateB $ y' - 14)

serialRate :: Word32
serialRate = 9600
