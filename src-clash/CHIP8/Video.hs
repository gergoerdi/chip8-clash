module CHIP8.Video where

import CHIP8.Types
import Clash.Prelude
import Cactus.Clash.Util

chipX :: Index 640 -> Maybe VidX
chipX x = let (x', _) = unpack . pack $ x :: (Unsigned 7, Unsigned 3)
          in enable (8 <= x' && x' < 8 + 64) (truncateB $ x' - 8)

chipY :: Index 480 -> Maybe VidY
chipY y = let (y', _) = unpack . pack $ y :: (Unsigned 6, Unsigned 3)
          in enable (14 <= y' && y' < 14 + 32) (truncateB $ y' - 14)
