{-# LANGUAGE NoStarIsType #-}
module CHIP8.Font (hexDigits) where

import Clash.Prelude
import Data.Word
import Data.Char
import qualified Data.List as L

-- | CHIP-8 bitmaps for hexadecimal digits, at 4x5 pixels. Padded to 8
-- pixel height to simplify indexing.
-- From http://laurencescotford.co.uk/?p=440
hexDigits :: Vec (16 * 8) Word8
hexDigits = concat . map (pad . fmap lineToByte) $
    ("****" :>
     "*  *" :>
     "*  *" :>
     "*  *" :>
     "****" :>
     Nil) :>

    ("  * " :>
     " ** " :>
     "  * " :>
     "  * " :>
     " ***" :>
     Nil) :>

    ("****" :>
     "   *" :>
     "****" :>
     "*   " :>
     "****" :>
     Nil) :>

    ("****" :>
     "   *" :>
     "****" :>
     "   *" :>
     "****" :>
     Nil) :>

    ("*  *" :>
     "*  *" :>
     "****" :>
     "   *" :>
     "   *" :>
     Nil) :>

    ("****" :>
     "*   " :>
     "****" :>
     "   *" :>
     "****" :>
     Nil) :>

    ("****" :>
     "*   " :>
     "****" :>
     "*  *" :>
     "****" :>
     Nil) :>

    ("****" :>
     "   *" :>
     "  * " :>
     " *  " :>
     " *  " :>
     Nil) :>

    ("****" :>
     "*  *" :>
     "****" :>
     "*  *" :>
     "****" :>
     Nil) :>

    ("****" :>
     "*  *" :>
     "****" :>
     "   *" :>
     "****" :>
     Nil) :>

    ("****" :>
     "*  *" :>
     "****" :>
     "*  *" :>
     "*  *" :>
     Nil) :>

    ("*** " :>
     "*  *" :>
     "*** " :>
     "*  *" :>
     "*** " :>
     Nil) :>

    ("****" :>
     "*   " :>
     "*   " :>
     "*   " :>
     "****" :>
     Nil) :>

    ("*** " :>
     "*  *" :>
     "*  *" :>
     "*  *" :>
     "*** " :>
     Nil) :>

    ("****" :>
     "*   " :>
     "****" :>
     "*   " :>
     "****" :>
     Nil) :>

    ("****" :>
     "*   " :>
     "****" :>
     "*   " :>
     "*   " :>
     Nil) :>
    Nil
  where
    pad = (++ repeat 0)

lineToByte :: String -> Word8
lineToByte s = L.foldl push 0 s `shiftL` 4
  where
    push :: Word8 -> Char -> Word8
    push x c = x `shiftL` 1 + if isSpace c then 0 else 1
