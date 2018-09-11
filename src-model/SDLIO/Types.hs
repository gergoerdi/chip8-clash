{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module SDLIO.Types
    ( KeypadState
    , VidX
    , VidY
    , Addr
    ) where

import Clash.Sized.Vector
import Clash.Sized.Unsigned

type KeypadState = Vec 16 Bool

type VidX = Unsigned 6
type VidY = Unsigned 5

type Addr = Unsigned 12
