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
data ISA
    = ADC    GPR GPR
    | ADD    GPR GPR
    | ADIW   GPR Imd
    | AND    GPR GPR
    | ANDI   GPR Wrd
    | ASR    GPR    
    | BCLR   Bit    
    | BLD    GPR Bit
    | BRBC   Bit BrK
    | BRBS   Bit BrK
    | BRCC   BrK    
    | BRCS   BrK    
    | BREAK         
    | BREQ   BrK    
    | BRGE   BrK    
    | BRHC   BrK    
    | BRHS   BrK    
    | BRID   BrK    
    | BRIE   BrK    
    | BRLO   BrK    
    | BRLT   BrK    
    | BRMI   BrK    
    | BRNE   BrK    
    | BRPL   BrK    
    | BRSH   BrK    
    | BRTC   BrK    
    | BRTS   BrK    
    | BRVC   BrK    
    | BRVS   BrK    
    | BSET   Bit    
    | BST    GPR Bit
    | CALL   Adr    
    | CBI    IOR Bit
    | CBR    GPR Wrd
    | CLC           
    | CLH           
    | CLI           
    | CLN           
    | CLR    GPR    
    | CLS           
    | CLT           
    | CLV           
    | CLZ           
    | COM    GPR    
    | CP     GPR GPR
    | CPC    GPR GPR
    | CPI    GPR Wrd
    | CPSE   GPR GPR
    | DEC    GPR    
    | DES    HWd    
    | EICALL        
    | EIJMP         
    | ELPM0          -- None, R0 implied 
    | ELPMZ  GPR Z   -- Rd and either Z or Z+
    | EOR    GPR GPR
    | FMUL   GPR GPR
    | FMULS  GPR GPR
    | FMULSU GPR GPR
    | ICALL         
    | IJMP          
    | IN     GPR IOR
    | INC    GPR    
    | JMP    Adr    
    | LDX    GPR X   -- LD using X register
    | LDDX   GPR LdQ -- LDD using X register
    | LDY    GPR Y   -- LD using Y register
    | LDDY   GPR LdQ -- LDD using Y register
    | LDI    GPR Imd
    | LDS    GPR Adr -- 16 bit address or 7 bit address
    | LPM0
    | LPMZ   GPR Z
    | LSL    GPR
    | LSR    GPR
    | MOV    GPR GPR
    | MOVW   GPR GPR -- {0,2..30} {0,2..30}
    | MUL    GPR GPR
    | MULS   GPR GPR
    | MULSU  GPR GPR -- {16 <= d <= 23} {16 <= r <= 23}
    | NEG    GPR
    | NOP
    | OR     GPR GPR
    | ORI    GPR Imd
    | OUT    IOR GPR
    | POP    GPR
    | PUSH   GPR
    | RCALL  Adr     -- Adr isn't really appropriate here
    | RET
    | RETI
    | RJUMP  Adr     -- Adr isn't really appropriate here
    | ROL    GPR
    | ROR    GPR
    | SBC    GPR GPR
    | SBCI   GPR Imd
    | SBI    IOR Bit
    | SBIC   IOR Bit
    | SBIS   IOR Bit
    | SBIW   GPR Wrd -- d {24,26,28,30}, 0 <= k <= 63

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
