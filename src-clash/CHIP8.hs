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

-- | 25.175 MHz clock, needed for the VGA mode we use.
-- CLaSH requires the clock period to be specified in picoseconds.
type Dom25 = Dom "CLK_25MHZ" (FromHz 25175000)

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
    board rxIn ps2Clk ps2Data = (txOut, (delay1 high vgaVSync, delay1 high vgaHSync, vgaR, vgaG, vgaB))
      where
        txOut = pure low

        VGADriver{..} = vgaDriver vga640x480at60
        ps2 = decodePS2 $ samplePS2 PS2{..}

        x0 = (chipX =<<) <$> vgaX
        y0 = (chipY =<<) <$> vgaY

        delay1 x = toSignal . delayed (singleton x) . fromSignal

        (dx, dy) = unbundle $ do
            key <- parseScanCode ps2
            pure $ case key of
                Just (ScanCode KeyPress 0xe075) -> (0, -1) -- up
                Just (ScanCode KeyPress 0xe072) -> (0, 1)  -- down
                Just (ScanCode KeyPress 0xe06b) -> (-1, 0) -- left
                Just (ScanCode KeyPress 0xe074) -> (1, 0)  -- right
                _ -> (0, 0)

        fbWrite = do
            x <- fix $ register 0 . (+ dx)
            y <- fix $ register 0 . (+ dy)
            pure $ Just (fbIndex x y, True)

        pixel = mux visible framebuf (pure False)
          where
            visible = isJust <$> x0 .&&. isJust <$> y0

            fbRead = fbIndex <$> (fromMaybe 0 <$> x0) <*> (fromMaybe 0 <$> y0)
            framebuf = blockRam (replicate (SNat :: SNat 2048) False) fbRead fbWrite

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

fbIndex :: VidX -> VidY -> Unsigned 11
fbIndex x y = unpack . pack $ (x, y)
