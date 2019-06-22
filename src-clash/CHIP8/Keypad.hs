{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}
module CHIP8.Keypad where

import CHIP8.Types

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.PS2
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad (guard)
import Data.Traversable (forM)
import Data.Foldable (find)

keypad
    :: (HiddenClockReset dom gated synchronous)
    => Signal dom (Maybe ScanCode) -> (Signal dom KeypadState, Signal dom (Maybe (Bool, Key)))
keypad scanCode = (keys, event)
  where
    event = (fromScanCode =<<) <$> scanCode

    keys = regMaybe (pure False) $ do
        ev <- event
        keys <- keys
        pure $ flip applyEvent keys <$> ev

applyEvent :: (Bool, Key) -> KeypadState -> KeypadState
applyEvent (state, key) = replace key state

fromScanCode :: ScanCode -> Maybe (Bool, Key)
fromScanCode (ScanCode ev code) = do
    (_, key) <- find ((code ==) . fst) (zip (concat codes) (concat layout))
    pure (ev == KeyPress, key)

layout :: Vec 4 (Vec 4 Key)
layout =
    (0x1 :> 0x2 :> 0x3 :> 0xc :> Nil) :>
    (0x4 :> 0x5 :> 0x6 :> 0xd :> Nil) :>
    (0x7 :> 0x8 :> 0x9 :> 0xe :> Nil) :>
    (0xa :> 0x0 :> 0xb :> 0xf :> Nil) :>
    Nil

codes :: Vec 4 (Vec 4 (Unsigned 9))
codes =
    (0x016 :> 0x01e :> 0x026 :> 0x025 :> Nil) :>
    (0x015 :> 0x01d :> 0x024 :> 0x02d :> Nil) :>
    (0x01c :> 0x01b :> 0x023 :> 0x02b :> Nil) :>
    (0x01a :> 0x022 :> 0x021 :> 0x02a :> Nil) :>
    Nil
