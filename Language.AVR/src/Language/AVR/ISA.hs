module Language.AVR.ISA where

import Language.AVR.Registers
import Language.AVR.Operands

{-
 - See: http://www.atmel.com/dyn/resources/prod_documents/doc0856.pdf
 -}

-- Definition of an Instruction Set Architecture
data ISA
    = ADC    GPR GPR
    | ADD    GPR GPR
    | ADIW   U4P K0_63
    | AND    GPR GPR
    | ANDI   U16 W8
    | ASR    GPR
    | BCLR   Bit
    | BLD    GPR Bit
    | BRBC   Bit Kn64_63
    | BRBS   Bit Kn64_63
    | BRCC   Kn64_63
    | BRCS   Kn64_63
    | BREAK
    | BREQ   Kn64_63
    | BRGE   Kn64_63
    | BRHC   Kn64_63
    | BRHS   Kn64_63
    | BRID   Kn64_63
    | BRIE   Kn64_63
    | BRLO   Kn64_63
    | BRLT   Kn64_63
    | BRMI   Kn64_63
    | BRNE   Kn64_63
    | BRPL   Kn64_63
    | BRSH   Kn64_63
    | BRTC   Kn64_63
    | BRTS   Kn64_63
    | BRVC   Kn64_63
    | BRVS   Kn64_63
    | BSET   Bit
    | BST    GPR Bit
    | CALL   A16
    | CBI    LIO Bit
    | CBR    U16 W8
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
    | CPI    U16 W8
    | CPSE   GPR GPR
    | DEC    GPR
    | DES    W4
    | EICALL
    | EIJMP
    | ELPM           -- ELPM
    | ELPMZ  GPR     -- ELPM Rd, Z
    | ELPMZP GPR Z   -- ELPM Rd, Z+
    | EOR    GPR GPR
    | FMUL   MRS MRS
    | FMULS  MRS MRS
    | FMULSU MRS MRS
    | ICALL
    | IJMP
    | IN     GPR IOS
    | INC    GPR
    | JMP    A22
    | LDX    GPR     -- LD Rd,X
    | LDXP   GPR     -- LD Rd,X+
    | LDXM   GPR     -- LD Rd,-X
    | LDY    GPR     -- LD Rd,Y
    | LDYP   GPR     -- LD Rd,Y+
    | LDYM   GPR     -- LD Rd,-Y
    | LDZ    GPR     -- LD Rd,Z
    | LDZP   GPR     -- LD Rd,Z+
    | LDZM   GPR     -- LD Rd,-Z
    | LDDY   GPR K0_63 -- LDD Rd,Y+q
    | LDDZ   GPR K0_63 -- LDD Rd,Z+q
    | LDI    U16 W8
    | LDS    GPR A16
    | LDS16  U16 A7
    | LPM            -- LPM
    | LPMZ   GPR     -- LPM Rd,Z
    | LPMZP  GPR     -- LPM Rd,Z+
    | LSL    GPR
    | LSR    GPR
    | MOV    GPR GPR
    | MOVW   RPS RPS -- {0,2..30} {0,2..30}
    | MUL    GPR GPR
    | MULS   U16 U16
    | MULSU  MRS MRS -- {16 <= d <= 23} {16 <= r <= 23}
    | NEG    GPR
    | NOP
    | OR     GPR GPR
    | ORI    U16 W8
    | OUT    IOS GPR
    | POP    GPR
    | PUSH   GPR
    | RCALL  Rel2K
    | RET
    | RETI
    | RJUMP  Rel2K
    | ROL    GPR
    | ROR    GPR
    | SBC    GPR GPR
    | SBCI   U16 W8
    | SBI    LIO Bit
    | SBIC   LIO Bit
    | SBIS   LIO Bit
    | SBIW   U4P K0_63 -- d {24,26,28,30}, 0 <= k <= 63
    | SBR    U16 W8
    | SBRC   GPR Bit
    | SBRS   GPR Bit
    | SEC
    | SEH
    | SEI
    | SEN
    | SER    U16
    | SES
    | SET
    | SEV
    | SEZ
    | SLEEP
    | SPM    -- I don't think this needs any operands? Manual ambiguous about Z+.
    | SPMZP  -- SPM Z+
    | STX    GPR -- ST X,Rr
    | STXP   GPR -- ST X+,Rr
    | STXM   GPR -- ST -X,Rr
    | STY    GPR -- ST Y,Rr
    | STYP   GPR -- ST Y+,Rr
    | STYM   GPR -- ST -Y,Rr
    | STZ    GPR -- ST Z,Rr
    | STZP   GPR -- ST Z+,Rr
    | STZM   GPR -- ST -Z,Rr
    | STDY   K0_63 GPR -- ST Y+q,Rr
    | STDZ   K0_63 GPR -- ST Z+q,Rr
    | STS    A16 GPR
    | STS16  A7  U16
    | SUB    GPR GPR
    | SUBI   U16 W8
    | SWAP   GPR
    | TST    GPR
    | WDR
    deriving (Show)
