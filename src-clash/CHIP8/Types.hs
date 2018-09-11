{-# LANGUAGE DataKinds #-}
module CHIP8.Types where

import Clash.Prelude
import Clash.Sized.Vector
import Clash.Sized.Unsigned

type Addr = Unsigned 12

type Key = Unsigned 16
type KeypadState = Vec 16 Bool

type VidX = Unsigned 6
type VidY = Unsigned 5
