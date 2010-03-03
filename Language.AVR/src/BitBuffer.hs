module BitBuffer where

import Data.Bits
import Data.Monoid
import Data.Word
import Data.List
import Numeric

data Bit = Zero | One
    deriving (Show, Enum, Bounded)

zero, one :: Bit
zero = Zero
one = One

type BaseBuffer = Word64

baseSize :: Int
baseSize = 64

op :: (Bits a) => Bit -> a -> Int -> a
op Zero = clearBit
op One  = setBit

data BitBuffer = BitBuffer {
    filled :: [BaseBuffer],
    current :: BaseBuffer,
    len :: Int
}

empty :: BitBuffer
empty = BitBuffer {
    filled = [],
    current = 0,
    len = 0
}

appendR :: Bit -> BitBuffer -> BitBuffer
appendR Zero (BitBuffer _ _ 0) = (BitBuffer [] 0 1)
appendR One  (BitBuffer _ _ 0) = (BitBuffer [] 1 1)
appendR bit  (BitBuffer f c l) = BitBuffer {
    filled = if full
                then f ++ [c]
                else f,
    current = if full
                then (op bit) 0 0
                else (op bit) (shiftL c 1) 0,
    len = l + 1
}
    where
        full = 0 == l `mod` baseSize

appendL :: Bit -> BitBuffer -> BitBuffer
appendL Zero (BitBuffer _ _ 0) = (BitBuffer [] 0 1)
appendL One  (BitBuffer _ _ 0) = (BitBuffer [] 1 1)
appendL bit  b@(BitBuffer f c l) | l > baseSize = appF
                                 | otherwise    = appC
    where
        o = op bit
        rem = l `mod` baseSize
        full = 0 == rem
        aL v = (op bit) v rem
        opF = b { filled = (o 0 0) : f, len = l + 1 }
        appF = if full
                    then opF
                    else BitBuffer {
                            filled = (aL (head f)) : (tail f),
                            current = c,
                            len = l + 1
                        }
        appC = if full
                    then BitBuffer {
                            filled = (o 0 0) : f,
                            current = c,
                            len = l + 1
                         }
                    else BitBuffer {
                            filled = f,
                            current = aL c,
                            len = l + 1
                         }

{-
toBitBuffer :: (Bits a) => a -> BitBuffer
toBitBuffer v = 
    where go v b i = 

fromBitBuffer :: (Bits b) => BitBuffer -> b
-}
    
showBin :: (Integral a) => a -> String
showBin v = pad baseSize '0' $ showIntAtBase 2 (['0'..'9'] !!) v ""

pad :: Int -> a -> [a] -> [a]
pad endLen padder initList = let c = endLen - length initList
                             in replicate c padder ++ initList

instance Show BitBuffer where
    show (BitBuffer f c l) = let p = concatMap showBin f ++ (showBin c)
                             in  drop (baseSize - (l `mod` baseSize)) p

showVerbose :: BitBuffer -> String
showVerbose (BitBuffer f c l) = concat [
        "(BitBuffer { filled = ",
        "[" ++ (intercalate "," (map showBin f)) ++ "]",
        ", current = ",
        showBin c,
        ", len = ",
        show l,
        "})"
    ]
