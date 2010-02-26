module Language.AVR.Operands (
    Bit(..),
    W8, w8,
    W4, w4,
    K0_63, k0_63,
    Kn64_63, kn64_63,
    A7,a7,
    A16,a16,
    A22,a22,
    Rel2K,rel2k,
) where

import Data.Word
import Data.Int

data Bit = BZero | BOne  | BTwo | BThree
         | BFour | BFive | BSix | BSeven
    deriving (Enum,Show)

data W8 = W8 Word8
    deriving (Show)

w8 :: Word8 -> W8
w8 = W8

data W4 = W4 Word8
    deriving (Show)

w4 :: Word8 -> W4
w4 v = if and [0x00 <= v, v <= 0x0F]
        then W4 v
        else oor v

data K0_63 = K0_63 Word8
    deriving (Show)

k0_63 :: Word8 -> K0_63
k0_63 v = if and [0 <= v, v <= 63]
            then K0_63 v
            else oor v

data Kn64_63 = Kn64_63 Int8
    deriving (Show)

kn64_63 :: Int8 -> Kn64_63
kn64_63 v = if and [(-64) <= v, v <= 63]
                then Kn64_63 v
                else oor v

data A7 = A7 Word8 -- 7 bit address
    deriving (Show)

a7 :: Word8 -> A7
a7 v = if and [0 <= v, v <= 127]
        then A7 v
        else oor v

data A16 = A16 Word16 -- 16 bit address
    deriving (Show)

a16 :: Word16 -> A16
a16 = A16

data A22 = A22 Word32 -- 22 bit address
    deriving (Show)

a22 :: Word32 -> A22
a22 = A22

oor :: (Show a) => a -> b
oor v = error $ "Value out of range: " ++ show v

data Rel2K = Rel2K Int32
    deriving (Show)

rel2k :: Int32 -> Rel2K
rel2k v = if and [(-2048) <= v, v <= 2048]
            then Rel2K v
            else oor v

