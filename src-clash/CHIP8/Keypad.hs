{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ApplicativeDo #-}
module CHIP8.Keypad where

import CHIP8.Types

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.PS2
import Data.Word
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad (guard)
import Data.Traversable (forM)
import Data.Foldable (find)

keypad
    :: (HiddenClockReset dom gated synchronous)
    => Signal dom (Maybe ScanCode) -> (Signal dom KeypadState, Signal dom (Maybe (KeyEvent, Key)))
keypad scanCode = (keys, event)
  where
    event = (fromScanCode =<<) <$> scanCode

    keys = regMaybe (pure False) $ do
        ev <- event
        keys <- keys
        pure $ flip applyEvent keys <$> ev

applyEvent :: (KeyEvent, Key) -> KeypadState -> KeypadState
applyEvent (ev, key) = replace key (ev == KeyPress)

fromScanCode :: ScanCode -> Maybe (KeyEvent, Key)
fromScanCode (ScanCode ev ext code) = do
    guard $ ext == False
    (_, key) <- find ((code ==) . fst) (zip (concat codes) (concat layout))
    pure (ev, key)

layout :: Vec 4 (Vec 4 Key)
layout =
    (0x1 :> 0x2 :> 0x3 :> 0xc :> Nil) :>
    (0x4 :> 0x5 :> 0x6 :> 0xd :> Nil) :>
    (0x7 :> 0x8 :> 0x9 :> 0xe :> Nil) :>
    (0xa :> 0x0 :> 0xb :> 0xf :> Nil) :>
    Nil

codes :: Vec 4 (Vec 4 Word8)
codes =
    (0x16 :> 0x1e :> 0x26 :> 0x25 :> Nil) :>
    (0x15 :> 0x1d :> 0x24 :> 0x2d :> Nil) :>
    (0x1c :> 0x1b :> 0x23 :> 0x2b :> Nil) :>
    (0x1a :> 0x22 :> 0x21 :> 0x2a :> Nil) :>
    Nil
