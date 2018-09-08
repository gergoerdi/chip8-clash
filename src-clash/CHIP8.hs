{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
          , PortName "BUTTON_UP"
          , PortName "BUTTON_DOWN"
          , PortName "BUTTON_LEFT"
          , PortName "BUTTON_RIGHT"
          ]
    , t_output = PortProduct ""
          [ PortName "TX"
          , PortProduct "" [ PortName "VGA_VSYNC", PortName "VGA_HSYNC", PortName "VGA_RED", PortName "VGA_GREEN", PortName "VGA_BLUE" ]
          ]
    }) #-}
topEntity
    :: Clock Dom25 Source
    -> Reset _ Asynchronous
    -> Signal Dom25 Bit
    -> Signal _ Bit
    -> Signal _ Bit
    -> Signal _ Bit
    -> Signal _ Bit
    -> Signal _ Bit
    -> Signal _ Bit
    -> ( Signal _ Bit
      , ( Signal _ Bit
        , Signal _ Bit
        , Signal _ (Unsigned 4)
        , Signal _ (Unsigned 4)
        , Signal _ (Unsigned 4)
        )
      )
topEntity = exposeClockReset board
  where
    board rxIn ps2Clk ps2Data up0 down0 left0 right0 = (txOut, (vgaVSync, vgaHSync, vgaR, vgaG, vgaB))
      where
        VGADriver{..} = vgaDriver vga640x480at60

        vgaX' = (chipX =<<) <$> vgaX
        vgaY' = (chipY =<<) <$> vgaY
        visible = isJust <$> vgaX' .&&. isJust <$> vgaY'

        ps2 = decodePS2 $ samplePS2 PS2{..}

        -- txOut = pure low
        -- txOut = tx clkRate serialRate $ enable <$> keyRead <*> key

        (txOut, _, _) = df (hideClockReset fifoDF d1 Nil `seqDF` txDF serialRate) (fromMaybe 0 <$> ps2) (isJust <$> ps2) (pure True)

        -- (dx, dy) = unbundle $ mealy

        button raw = isRising maxBound $ debounce d16 False $ bitToBool <$> raw

        up = button up0
        down = button down0
        left = button left0
        right = button right0

        dx = mux left (-1) $
             mux right 1 $
             0

        x0 = register 0 (x0 + dx)

        dy = mux up (-1) $
             mux down 1 $
             0

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

txDF
    :: (HiddenClockReset domain gated synchronous, domain ~ Dom s ps, KnownNat ps)
    => Word32 -> DataFlow domain Bool Bool Word8 Bit
txDF serialRate = liftDF $ \input inValid inReady ->
    let TXOut{..} = tx serialRate (enable <$> (inValid .&&. inReady) <*> input)
        txValid = pure True
    in (txOut, txValid, txReady)
