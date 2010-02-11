{-# LANGUAGE GADTs #-}
module AVR where

import Text.Printf

{-
 - See: http://www.atmel.com/dyn/resources/prod_documents/doc0856.pdf
 -}

-- General Purpose Register
data GPR = R00 | R01 | R02 | R03 | R04 | R05 | R06 | R07 | R08 | R09
         | R10 | R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19
         | R20 | R21 | R22 | R23 | R24 | R25 | R26 | R27 | R28 | R29
         | R30 | R31
    deriving (Enum)

instance Show GPR where
    show r = "r" ++ show (fromEnum r :: Int)

-- IO Register
data IOR = IOR
    deriving (Enum,Show)

-- Z register: ZP -> Z+
data Z = Z | ZP 
    deriving (Enum,Show)

-- X register: XM -> X-, XP -> X+
data X = XM | X | XP
    deriving (Enum,Show)

-- Y register: YM -> Y-, YP -> Y+
data Y = YM | Y | YP
    deriving (Enum,Show)

type XY = Either X Y
        
data Bit = BZero | BOne  | BTwo | BThree
         | BFour | BFive | BSix | BSeven
    deriving (Enum,Show)

-- Immediate
data Imd = Imd Int
instance Show Imd where
    show (Imd v) = show v

-- Relative branch offset
data BrK = BrK Int deriving (Show)

mkBrK :: Int -> BrK
mkBrK k = if (-64) <= k && k <= 63
            then BrK k
            else error $ "Invalid offset value: " ++ (show k)

-- Program Memory Address
data Adr = Adr Int deriving (Show)

mkAdr16 :: Int -> Adr
mkAdr16 a = if 0 <= a && a < (2^16)
                then Adr a
                else error $ "Invalid address value: " ++ (show a)

mkAdr22 :: Int -> Adr
mkAdr22 a = if 0 <= a && a < (2^22)
                then Adr a
                else error $ "Invalid address value: " ++ (show a)

data LdQ = LdQ Int deriving (Show)

mkLdQ :: Int -> LdQ
mkLdQ q = if 0 <= q && q <= 63
            then LdQ q
            else error $ "Invalid load offset value: " ++ (show q)


-- 8 bit word
data Wrd = Wrd Int
    deriving (Show)

-- 4 bit half-word
data HWd = HWd Int
    deriving (Show)

mkHWd :: Int -> HWd
mkHWd k = if 0x00 <= k && k <= 0x0F
            then HWd k
            else error $ "I:nvalid half word: " ++ (show k)

-- Definition of an Instruction Set Architecture
data ISA where {
    ADC    :: GPR -> GPR -> ISA;
    ADD    :: GPR -> GPR -> ISA;
    ADIW   :: GPR -> Imd -> ISA;
    AND    :: GPR -> GPR -> ISA;
    ANDI   :: GPR -> Wrd -> ISA;
    ASR    :: GPR        -> ISA;
    BCLR   :: Bit        -> ISA;
    BLD    :: GPR -> Bit -> ISA;
    BRBC   :: Bit -> BrK -> ISA;
    BRBS   :: Bit -> BrK -> ISA;
    BRCC   :: BrK        -> ISA;
    BRCS   :: BrK        -> ISA;
    BREAK  ::               ISA;
    BREQ   :: BrK        -> ISA;
    BRGE   :: BrK        -> ISA;
    BRHC   :: BrK        -> ISA;
    BRHS   :: BrK        -> ISA;
    BRID   :: BrK        -> ISA;
    BRIE   :: BrK        -> ISA;
    BRLO   :: BrK        -> ISA;
    BRLT   :: BrK        -> ISA;
    BRMI   :: BrK        -> ISA;
    BRNE   :: BrK        -> ISA;
    BRPL   :: BrK        -> ISA;
    BRSH   :: BrK        -> ISA;
    BRTC   :: BrK        -> ISA;
    BRTS   :: BrK        -> ISA;
    BRVC   :: BrK        -> ISA;
    BRVS   :: BrK        -> ISA;
    BSET   :: Bit        -> ISA;
    BST    :: GPR -> Bit -> ISA;
    CALL   :: Adr        -> ISA;
    CBI    :: IOR -> Bit -> ISA;
    CBR    :: GPR -> Wrd -> ISA;
    CLC    ::               ISA;
    CLH    ::               ISA;
    CLI    ::               ISA;
    CLN    ::               ISA;
    CLR    :: GPR        -> ISA;
    CLS    ::               ISA;
    CLT    ::               ISA;
    CLV    ::               ISA;
    CLZ    ::               ISA;
    COM    :: GPR        -> ISA;
    CP     :: GPR -> GPR -> ISA;
    CPC    :: GPR -> GPR -> ISA;
    CPI    :: GPR -> Wrd -> ISA;
    CPSE   :: GPR -> GPR -> ISA;
    DEC    :: GPR        -> ISA;
    DES    :: HWd        -> ISA;
    EICALL ::               ISA;
    EIJMP  ::               ISA;
    ELPM0  ::               ISA; -- None, R0 implied 
    ELPMZ  :: GPR -> Z   -> ISA; -- Rd and either Z or Z+
    EOR    :: GPR -> GPR -> ISA;
    FMUL   :: GPR -> GPR -> ISA;
    FMULS  :: GPR -> GPR -> ISA;
    FMULSU :: GPR -> GPR -> ISA;
    ICALL  ::               ISA;
    IJMP   ::               ISA;
    IN     :: GPR -> IOR -> ISA;
    INC    :: GPR        -> ISA;
    JMP    :: Adr        -> ISA;
    LDX    :: GPR -> X   -> ISA; -- LD using X register
    LDDX   :: GPR -> LdQ -> ISA; -- LDD using X register
    LDY    :: GPR -> Y   -> ISA; -- LD using Y register
    LDDY   :: GPR -> LdQ -> ISA; -- LDD using Y register
    LDI    :: GPR -> Wrd -> ISA
}

instance Show ISA where
    show (ADC  d r) = prtIS2 "adc"  d r
    show (ADD  d r) = prtIS2 "add"  d r
    show (ADIW d k) = prtIS2 "adiw" d k

st6_0, st6_1, st6_2 :: String
st6_0 = "%-6s"
st6_1 = "%-6s %s"
st6_2 = "%-6s %s,%s"

prtIS0 :: String -> String
prtIS0 = printf st6_0

prtIS1 :: (Show a) => String -> a -> String
prtIS1 n d = printf st6_1 n (show d)

prtIS2 :: (Show a, Show b) => String -> a -> b -> String
prtIS2 n d r = printf st6_2 n (show d) (show r)
