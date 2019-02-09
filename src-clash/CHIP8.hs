{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NumericUnderscores #-}
module CHIP8 where

import CHIP8.Types
import CHIP8.CPU
import CHIP8.Video
import CHIP8.Keypad
import CHIP8.Font

import Clash.Prelude hiding (clkPeriod)
import Cactus.Clash.Util
import Cactus.Clash.Clock
-- import Cactus.Clash.SerialTX
-- import Cactus.Clash.SerialRX
import Cactus.Clash.VGA
import Cactus.Clash.PS2
import Cactus.Clash.CPU
import Data.Word
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad (guard)
import Data.Function

-- | 25.175 MHz clock, needed for the VGA mode we use.
-- CLaSH requires the clock period to be specified in picoseconds.
type Dom25 = Dom "CLK_25MHZ" (FromHz 25_175_000)

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "CHIP8"
    , t_inputs =
          [ PortName "CLK_25MHZ"
          , PortName "RESET"
          -- , PortName "RX"
          , PortName "PS2_CLK"
          , PortName "PS2_DATA"
          ]
    , t_output = PortProduct ""
          -- [ PortName "TX"
          -- , PortProduct ""
            [ PortName "VGA_VSYNC"
            , PortName "VGA_HSYNC"
            , PortName "VGA_DE"
            , PortName "VGA_RED"
            , PortName "VGA_GREEN"
            , PortName "VGA_BLUE"
            ]
          -- ]
    }) #-}
topEntity
    :: Clock Dom25 Source
    -> Reset Dom25 Asynchronous
    -- -> Signal Dom25 Bit
    -> Signal Dom25 Bit
    -> Signal Dom25 Bit
    -> ( -- Signal Dom25 Bit
       ( Signal Dom25 Bit
        , Signal Dom25 Bit
        , Signal Dom25 Bool
        , Signal Dom25 (Unsigned 8)
        , Signal Dom25 (Unsigned 8)
        , Signal Dom25 (Unsigned 8)
        )
      )
topEntity = exposeClockReset board
  where
    board {- rxIn -} ps2Clk ps2Data = ({- txOut, -} (delay1 high vgaVSync, delay1 high vgaHSync, delay1 False vgaVisible, vgaR, vgaG, vgaB))
      where
        -- txOut = pure low

        VGADriver{..} = vgaDriver vga640x480at60
        vgaVisible = (isJust <$> vgaX) .&&. (isJust <$> vgaY)
        ps2 = decodePS2 $ samplePS2 PS2{..}

        x0 = (chipX =<<) <$> vgaX
        y0 = (chipY =<<) <$> vgaY

        framebuf r = blockRam (replicate d2048 low) r' w
          where
            r' = fbIndex <$> r
            w = packWrite (fbIndex <$> cpuOutFBAddr <$> cpuOut) (cpuOutFBWrite <$> cpuOut)

        cpuIn = do
            cpuInFB <- framebuf $ cpuOutFBAddr <$> cpuOut
            cpuInMem <- memRead memAddr
            cpuInKeys <- keys
            cpuInKeyEvent <- keyEvent
            cpuInVBlank <- delay1 False vgaStartFrame
            pure CPUIn{..}
          where
            (keys, keyEvent) = keypad $ parseScanCode ps2
            memAddr = cpuOutMemAddr <$> cpuOut
            memWrite = fmap pack <$> cpuOutMemWrite <$> cpuOut

            fontROM = rom $(lift hexDigits)
            mainRAM addr = unpack <$> blockRamFile d4096 "image.hex" addr (packWrite addr memWrite)

            memRead = memoryMap $
                UpTo 0x0200 fontROM $
                Default mainRAM

        cpuOut = mealyState (runCPU defaultOut cpu) initState cpuIn

        pixel = mux visible (framebuf fbRead) (pure low)
          where
            visible = isJust <$> x0 .&&. isJust <$> y0
            fbRead = (,) <$> (fromMaybe 0 <$> x0) <*> (fromMaybe 0 <$> y0)

        vgaR = monochrome <$> pixel
        vgaG = monochrome <$> pixel
        vgaB = monochrome <$> pixel

type MemRead domain a b = Signal domain (Unsigned a) -> Signal domain b

data MemSpec domain a b
    = UpTo (Unsigned a) (MemRead domain a b) (MemSpec domain a b)
    | Default (MemRead domain a b)

delay1
    :: (Undefined a, HiddenClockReset domain gated synchronous)
    => a -> Signal domain a -> Signal domain a
delay1 x = toSignal . delayed (singleton x) . fromSignal

memoryMap
    :: (KnownNat a, HiddenClockReset domain gated synchronous)
    => MemSpec domain a b
    -> Signal domain (Unsigned a)
    -> Signal domain b
memoryMap mems addr = go mems
  where
    addr' = delay1 0 addr
    go (UpTo lim mem mems) = mux (addr' .<. pure lim) (mem addr) $ go mems
    go (Default mem) = mem addr

monochrome :: (Bounded a) => Bit -> a
monochrome b = if bitToBool b then maxBound else minBound

serialRate :: Word32
serialRate = 9600

fbIndex :: (VidX, VidY) -> Unsigned 11
fbIndex = unpack . pack

packWrite :: (Applicative f) => f a -> f (Maybe b) -> f (Maybe (a, b))
packWrite addr x = sequenceA <$> ((,) <$> addr <*> x)

d2048 :: SNat 2048
d2048 = SNat

d4096 :: SNat 4096
d4096 = SNat
