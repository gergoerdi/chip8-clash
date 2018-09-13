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

keypad
    :: (HiddenClockReset dom gated synchronous)
    => Signal dom (Maybe ScanCode) -> (Signal dom KeypadState, Signal dom (Maybe (KeyEvent, Key)))
keypad rawEvent = (keys, event)
  where
    event = do
        rawEvent <- rawEvent
        -- pure $ forM rawEvent $ \(ScanCode ev ext code) -> case (ext, code) of
        pure undefined

    keys = regMaybe (pure False) $ do
        ev <- event
        keys <- keys
        return $ applyEvent <$> ev <*> pure keys

applyEvent :: (KeyEvent, Key) -> KeypadState -> KeypadState
applyEvent (ev, key) = replace key (ev == KeyPress)
