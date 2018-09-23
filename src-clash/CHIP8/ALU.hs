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

-- | 9-bit maximal linear feedback shift register based on x^9 + x^5 + 1
-- http://en.wikipedia.org/wiki/Linear_feedback_shift_register#Some_polynomials_for_maximal_LFSRs
lfsr :: Unsigned 9 -> Unsigned 9
lfsr s = (s `rotateR` 1) `xor` b4
  where
    b = fromIntegral $ complement . lsb $ s
    b4 = b `shiftL` 4
