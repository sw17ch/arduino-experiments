module BitBuffer where

import Data.Bits
import Data.Monoid
import Data.Word
import Numeric

data Bit = Zero | One
    deriving (Show, Enum, Bounded)

zero, one :: Bit
zero = Zero
one = One

data BitBuffer = BitBuffer {
    filled :: [Word64],
    current :: Word64,
    len :: Word64
}

empty :: BitBuffer
empty = BitBuffer {
    filled = [],
    current = 0,
    len = 0
}

appendBit :: Bit -> BitBuffer -> BitBuffer
appendBit Zero (BitBuffer _ _ 0) = (BitBuffer [] 0 1)
appendBit One  (BitBuffer _ _ 0) = (BitBuffer [] 1 1)
appendBit bit  (BitBuffer f c l) = BitBuffer {
    filled = if full
                then f ++ [c]
                else f,
    current = if full
                then op 0 0
                else op (shiftL c 1) 0,
    len = l + 1
}
    where
        full = 0 == l `mod` 64
        op = case bit of
                Zero -> clearBit
                One  -> setBit

{-
toBitBuffer :: (Bits a) => a -> BitBuffer
toBitBuffer v = 
    where go v b i = 

fromBitBuffer :: (Bits b) => BitBuffer -> b
-}
    
showBin :: (Integral a) => a -> String
showBin v = showIntAtBase 2 (['0'..'9'] !!) v ""

instance Show BitBuffer where
    show (BitBuffer f c _) = concatMap showBin f ++ (showBin c)
