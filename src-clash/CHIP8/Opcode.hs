{-# LANGUAGE PatternSynonyms #-}
module CHIP8.Opcode
       ( Fun(..), Op(..)
       , decode
       ) where

import Clash.Prelude

import CHIP8.Types
import Data.Word
import Text.Printf

data Fun = Id
         | Or
         | And
         | XOr
         | Add
         | Subtract
         | ShiftRight
         | SubtractFlip
         | ShiftLeft
         deriving (Show)

data Op = ClearScreen
        | Ret
        | Sys Addr
        | Jump Addr
        | Call Addr
        | SkipEqImm Reg Word8 Bool -- True if skip on eq
        | SkipEqReg Reg Reg Bool
        | PutImm Reg Word8
        | AddImm Reg Word8
        | Move Reg Reg Fun
        | SetPtr Addr
        | JumpPlusR0 Addr
        | Randomize Reg Word8
        | DrawSprite Reg Reg Nybble
        | SkipKey Reg Bool -- True if skip on key pressed
        | GetTimer Reg
        | WaitKey Reg
        | SetTimer Reg
        | SetSound Reg
        | AddPtr Reg
        | LoadFont Reg
        | StoreBCD Reg
        | StoreRegs Reg
        | LoadRegs Reg
        deriving (Show)

decode :: Word8 -> Word8 -> Op
decode hi lo = case codes of
    (0x0, 0x0, 0xe, 0x0) -> ClearScreen
    (0x0, 0x0, 0xe, 0xe) -> Ret
    (0x0,   _,   _,   _) -> Sys addr
    (0x1,   _,   _,   _) -> Jump addr
    (0x2,   _,   _,   _) -> Call addr
    (0x3,   x,   _,   _) -> SkipEqImm (reg x) imm True
    (0x4,   x,   _,   _) -> SkipEqImm (reg x) imm False
    (0x5,   x,   y, 0x0) -> SkipEqReg (reg x) (reg y) True
    (0x6,   x,   _,   _) -> PutImm (reg x) imm
    (0x7,   x,   _,   _) -> AddImm (reg x) imm
    (0x8,   x,   y, fun) -> Move (reg x) (reg y) (decodeFun fun)
    (0x9,   x,   y, 0x0) -> SkipEqReg (reg x) (reg y) False
    (0xa,   _,   _,   _) -> SetPtr addr
    (0xb,   _,   _,   _) -> JumpPlusR0 addr
    (0xc,   x,   _,   _) -> Randomize (reg x) imm
    (0xd,   x,   y,   n) -> DrawSprite (reg x) (reg y) n
    (0xe,   x, 0x9, 0xe) -> SkipKey (reg x) True
    (0xe,   x, 0xa, 0x1) -> SkipKey (reg x) False
    (0xf,   x, 0x0, 0x7) -> GetTimer (reg x)
    (0xf,   x, 0x0, 0xa) -> WaitKey (reg x)
    (0xf,   x, 0x1, 0x5) -> SetTimer (reg x)
    (0xf,   x, 0x1, 0x8) -> SetSound (reg x)
    (0xf,   x, 0x1, 0xe) -> AddPtr (reg x)
    (0xf,   x, 0x2, 0x9) -> LoadFont (reg x)
    (0xf,   x, 0x3, 0x3) -> StoreBCD (reg x)
    (0xf,   x, 0x5, 0x5) -> StoreRegs (reg x)
    (0xf,   x, 0x6, 0x5) -> LoadRegs (reg x)
    _                    -> errorX $ "Unknown opcode: " <> unwords [show a1, show a2, show a3, show a4]
  where
    (a1, a2) = nybbles hi
    (a3, a4) = nybbles lo
    codes = (a1, a2, a3, a4)
    addr = toAddr a2 a3 a4
    imm = lo

    reg = unpack . pack

    decodeFun :: Nybble -> Fun
    decodeFun 0x0 = Id
    decodeFun 0x1 = Or
    decodeFun 0x2 = And
    decodeFun 0x3 = XOr
    decodeFun 0x4 = Add
    decodeFun 0x5 = Subtract
    decodeFun 0x6 = ShiftRight
    decodeFun 0x7 = SubtractFlip
    decodeFun 0xe = ShiftLeft
    decodeFun n = fatal "Unknown Move function" n

fatal :: (Show a) => String -> a -> b
fatal s x = errorX $ s <> ": " <> show x
