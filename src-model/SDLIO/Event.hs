{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module SDLIO.Event
    ( UserEvent(..)
    , userEvent
    ) where

import Clash.Prelude

import CHIP8.Types
import CHIP8.Keypad (layout)
import SDL

import Clash.Sized.Vector as V
import Clash.Sized.Index
import Data.List as L

data UserEvent
    = KeypadEvent Bool (Index 16)
    | Quit

userEvent :: EventPayload -> Maybe UserEvent
userEvent ev = case ev of
    KeyboardEvent KeyboardEventData{keyboardEventKeysym = Keysym{..}, ..} ->
        case (keyboardEventKeyMotion, keysymKeycode) of
            (Pressed, KeycodeEscape) -> Just Quit
            (motion, key) | Just i <- encodeKey key -> Just $ KeypadEvent (motion == Pressed) i
            _ -> Nothing
    WindowClosedEvent{} -> Just Quit
    _ -> Nothing

keyMap :: Vec 4 (Vec 4 Keycode)
keyMap =
    (Keycode1 :> Keycode2 :> Keycode3 :> Keycode4 :> Nil) :>
    (KeycodeQ :> KeycodeW :> KeycodeE :> KeycodeR :> Nil) :>
    (KeycodeA :> KeycodeS :> KeycodeD :> KeycodeF :> Nil) :>
    (KeycodeZ :> KeycodeX :> KeycodeC :> KeycodeV :> Nil) :>
    Nil

encodeKey :: Keycode -> Maybe (Index 16)
encodeKey = flip lookup table
  where
    table :: [(Keycode, Index 16)]
    table = L.concatMap V.toList . V.toList $ V.zipWith V.zip keyMap layout
