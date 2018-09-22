module CHIP8.ALU where

import Clash.Prelude
import CHIP8.Types
import CHIP8.Opcode
import Data.Word

alu :: Fun -> Word8 -> Word8 -> (Word8, Maybe Word8)
alu fun = case fun of
    Id -> noCarry (\x y -> y)
    Or -> noCarry (.|.)
    And -> noCarry (.&.)
    XOr -> noCarry xor
    Add -> carry (+) (\x y z -> z < x)
    Subtract -> carry (-) (\x y z -> z <= x)
    SubtractFlip -> carry (flip (-)) (\x y z -> z <= y)
    ShiftRight -> carry (\x _ -> x `shiftR` 1) (\x _ _ -> x `testBit` 0)
    ShiftLeft -> carry (\x _ -> x `shiftL` 1) (\x _ _ -> x `testBit` 7)
  where
    noCarry f x y = (f x y, Nothing)
    carry f p x y = let z = f x y in (z, Just $ if p x y z then 1 else 0)

toBCD :: Word8 -> Vec 3 Word8
toBCD x =
    x `div` 100 :>
    (x `div` 10) `mod` 10 :>
    x `mod` 10 :>
    Nil

toFont :: Word8 -> Addr
toFont x = fromIntegral lo `shiftL` 3
  where
    (_, lo) = nybbles x

-- | 15-bit maximal linear feedback shift register based on x^15 + x^14 + 1
-- http://en.wikipedia.org/wiki/Linear_feedback_shift_register#Some_polynomials_for_maximal_LFSRs
lfsr :: Unsigned 15 -> Unsigned 15
lfsr s = (s `shiftR` 1) .|. (fromIntegral b `shiftL` 14)
  where
    b = complement $ s!0 `xor` s!1
