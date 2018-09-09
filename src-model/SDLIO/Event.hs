{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module SDLIO.Event
    ( UserEvent(..)
    , userEvent
    ) where

import SDLIO.Types

import SDL
import Clash.Sized.Vector as V
import Clash.Sized.Index

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

keyMap :: Vec 4 (Vec 4 (Index 16))
keyMap =
    (0x1 :> 0x2 :> 0x3 :> 0xc :> Nil) :>
    (0x4 :> 0x5 :> 0x6 :> 0xd :> Nil) :>
    (0x7 :> 0x8 :> 0x9 :> 0xe :> Nil) :>
    (0xa :> 0x0 :> 0xb :> 0xf :> Nil) :>
    Nil

keyLayout :: Vec 4 (Vec 4 Keycode)
keyLayout =
    (Keycode1 :> Keycode2 :> Keycode3 :> Keycode4 :> Nil) :>
    (KeycodeQ :> KeycodeW :> KeycodeE :> KeycodeR :> Nil) :>
    (KeycodeA :> KeycodeS :> KeycodeD :> KeycodeF :> Nil) :>
    (KeycodeZ :> KeycodeX :> KeycodeC :> KeycodeV :> Nil) :>
    Nil

encodeKey :: Keycode -> Maybe (Index 16)
encodeKey = flip lookup table
  where
    table :: [(Keycode, Index 16)]
    table = concatMap V.toList . V.toList $ V.zipWith V.zip keyLayout keyMap
