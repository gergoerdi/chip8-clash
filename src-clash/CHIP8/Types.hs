{-# LANGUAGE DataKinds #-}
module CHIP8.Types where

import Clash.Prelude
import Clash.Sized.Vector
import Clash.Sized.Unsigned
import Data.Word

type Addr = Unsigned 12
type Nybble = Unsigned 4
type Reg = Index 16

type Key = Index 16
type KeypadState = Vec 16 Bool

type VidX = Unsigned 6
type VidY = Unsigned 5

nybbles :: Word8 -> (Nybble, Nybble)
nybbles = bitCoerce

toAddr :: Nybble -> Nybble -> Nybble -> Addr
toAddr a1 a2 a3 = bitCoerce (a1, a2, a3)
