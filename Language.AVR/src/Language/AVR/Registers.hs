module Language.AVR.Registers where

class ToGPR a where
    toGPR :: a -> GPR

-- General Purpose Register
data GPR = R00 | R01 | R02 | R03 | R04 | R05 | R06 | R07 | R08 | R09
         | R10 | R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19
         | R20 | R21 | R22 | R23 | R24 | R25 | R26 | R27 | R28 | R29
         | R30 | R31
    deriving (Enum)

instance ToGPR GPR where
    toGPR = id

instance Show GPR where
    show r = "r" ++ show (fromEnum r :: Int)

-- Low IO Register
data LIO = I00 | I01 | I02 | I03 | I04 | I05 | I06 | I07 | I08 | I09
         | I10 | I11 | I12 | I13 | I14 | I15 | I16 | I17 | I18 | I19
         | I20 | I21 | I22 | I23 | I24 | I25 | I26 | I27 | I28 | I29
         | I30 | I31
    deriving (Enum)

instance Show LIO where
    show i = "$" ++ show (fromEnum i :: Int)

-- IO Space Register
data IOS = A00 | A01 | A02 | A03 | A04 | A05 | A06 | A07
         | A08 | A09 | A10 | A11 | A12 | A13 | A14 | A15
         | A16 | A17 | A18 | A19 | A20 | A21 | A22 | A23
         | A24 | A25 | A26 | A27 | A28 | A29 | A30 | A31
         | A32 | A33 | A34 | A35 | A36 | A37 | A38 | A39
         | A40 | A41 | A42 | A43 | A44 | A45 | A46 | A47
         | A48 | A49 | A50 | A51 | A52 | A53 | A54 | A55
         | A56 | A57 | A58 | A59 | A60 | A61 | A62 | A63
    deriving (Enum)

instance Show IOS where
    show i = "$" ++ show (fromEnum i :: Int)

-- Special Registers
data X = X deriving (Show)
data Y = Y deriving (Show)
data Z = Z deriving (Show)

-- Special Register Pairs
data XP = XL | XH deriving (Show)
data YP = YL | YH deriving (Show)
data ZP = ZL | ZH deriving (Show)

-- SRAM Pointer Register (X or Y)
data SPR = XR X | YR Y
    deriving (Show)

-- Program Memory Pointer Register
data PPR = ZR Z
    deriving (Show)

-- Upper 4 register pairs
data U4P = U4P_24 | U4P_26
         | U4P_28 | U4P_30

instance Show U4P where
    show = show . toGPR

instance ToGPR U4P where
    toGPR U4P_24 = R24
    toGPR U4P_26 = R26
    toGPR U4P_28 = R28
    toGPR U4P_30 = R30

-- Register Pairs (every other register)
-- RP_00 -> r1:r0
data RPS = RP_00 | RP_02 | RP_04 | RP_06
         | RP_08 | RP_10 | RP_12 | RP_14
         | RP_16 | RP_18 | RP_20 | RP_22
         | RP_24 | RP_26 | RP_28 | RP_30

instance Show RPS where
    show = show . toGPR

instance ToGPR RPS where
    toGPR RP_00 = R00
    toGPR RP_02 = R02
    toGPR RP_04 = R04
    toGPR RP_06 = R06
    toGPR RP_08 = R08
    toGPR RP_10 = R10
    toGPR RP_12 = R12
    toGPR RP_14 = R14
    toGPR RP_16 = R16
    toGPR RP_18 = R18
    toGPR RP_20 = R20
    toGPR RP_22 = R22
    toGPR RP_24 = R24
    toGPR RP_26 = R26
    toGPR RP_28 = R28
    toGPR RP_30 = R30

data U16 = U16_16 | U16_17 | U16_18 | U16_19 | U16_20 | U16_21 | U16_22 | U16_23
         | U16_24 | U16_25 | U16_26 | U16_27 | U16_28 | U16_29 | U16_30 | U16_31

instance Show U16 where
    show = show . toGPR

instance ToGPR U16 where
    toGPR U16_16 = R16
    toGPR U16_17 = R17
    toGPR U16_18 = R18
    toGPR U16_19 = R19
    toGPR U16_20 = R20
    toGPR U16_21 = R21
    toGPR U16_22 = R22
    toGPR U16_23 = R23
    toGPR U16_24 = R24
    toGPR U16_25 = R25
    toGPR U16_26 = R26
    toGPR U16_27 = R27
    toGPR U16_28 = R28
    toGPR U16_29 = R29
    toGPR U16_30 = R30
    toGPR U16_31 = R31

-- Multiply Registers
data MRS = F16 | F17 | F18 | F19
         | F20 | F21 | F22 | F23

instance Show MRS where
    show = show . toGPR

instance ToGPR MRS where
    toGPR F16 = R16
    toGPR F17 = R17
    toGPR F18 = R18
    toGPR F19 = R19
    toGPR F20 = R20
    toGPR F21 = R21
    toGPR F22 = R22
    toGPR F23 = R23
