module SPARC.CodeGen.Amode (
        getAmode
)

where

import GhcPrelude

import {-# SOURCE #-} SPARC.CodeGen.Gen
import SPARC.CodeGen.Base
import SPARC.AddrMode
import SPARC.Imm
import SPARC.Instr
import SPARC.Regs()
import SPARC.Base
import NCGMonad
import Format
import Platform

import Cmm
import CLabel

import DynFlags
import OrdList


-- | Generate code to reference a memory address.
getAmode
        :: CmmExpr      -- ^ expr producing an address
        -> NatM Amode

getAmode tree@(CmmRegOff _ _)
    = do dflags <- getDynFlags
         getAmode (mangleIndexTree dflags tree)

getAmode (CmmMachOp (MO_Sub _) [x, CmmLit (CmmInt i _)])
  | fits13Bits (-i)
  = do
       (reg, code) <- getSomeReg x
       let
         off  = ImmInt (-(fromInteger i))
       return (Amode (AddrRegImm reg off) code)


getAmode (CmmMachOp (MO_Add _) [x, CmmLit (CmmInt i _)])
  | fits13Bits i
  = do
       (reg, code) <- getSomeReg x
       let
         off  = ImmInt (fromInteger i)
       return (Amode (AddrRegImm reg off) code)

-- Specific case needed for PIC to avoid infinite recursion
getAmode (CmmMachOp (MO_Add width) [x@(CmmReg (CmmGlobal PicBaseReg)), CmmLit lit])
  | Just (GotSymbolPtr, _) <- litToDynamicLinkerLabelInfo lit
  = do let imm = litToImm lit
       (base, baseCode) <- getSomeReg x
       let reg_format = intFormat width
       reg <- getNewRegNat reg_format

       let code = baseCode `appOL` toOL [
                SETHI (GDOP_HIX22 imm) reg,
                XOR False reg (RIImm (GDOP_LOX10 imm)) reg]
           hint = GDOP imm

       return (Amode (AddrAddrHint (AddrRegReg base reg) hint) code)

  | Just (GotSymbolOffset, _) <- litToDynamicLinkerLabelInfo lit
  -- R_SPARC_GOTDATA_HIX22/LOX10 cannot be expressed in assembly, only the
  -- GOTDATA_OP variants, so we do the same as GotSymbolPtr but also perform
  -- the load. Specifying the hint adds an R_SPARC_GOTDATA relocation for the
  -- load itself, so the linker should optimise the sequence to use
  -- R_SPARC_GOTDATA_HIX22/LOX10 followed by an add instead.
  = do let imm = litToImm lit
       (base, baseCode) <- getSomeReg x
       let reg_format = intFormat width
       reg <- getNewRegNat reg_format

       let hint = GDOP imm
           code = baseCode `appOL` toOL [
                SETHI (GDOP_HIX22 imm) reg,
                XOR False reg (RIImm (GDOP_LOX10 imm)) reg,
                LD reg_format (AddrAddrHint (AddrRegReg base reg) hint) reg]

       return (Amode (AddrRegImm reg (ImmInt 0)) code)

getAmode (CmmMachOp (MO_Add _) [x, y])
  = do
    (regX, codeX) <- getSomeReg x
    (regY, codeY) <- getSomeReg y
    let
        code = codeX `appOL` codeY
    return (Amode (AddrRegReg regX regY) code)

getAmode (CmmLit lit)
  = do dflags <- getDynFlags
       let platform = targetPlatform dflags
           is32Bit = target32Bit platform

       if   is32Bit
       then getAmodeLit32 lit
       else getAmodeLit64 lit

getAmode other
  = do
       (reg, code) <- getSomeReg other
       let
            off  = ImmInt 0
       return (Amode (AddrRegImm reg off) code)

getAmodeLit32 :: CmmLit -> NatM Amode
getAmodeLit32 lit
  = do
        let imm = litToImm lit
        tmp     <- getNewRegNat II32

        let code = unitOL $ SETHI (HI imm) tmp

        return (Amode (AddrRegImm tmp (LO imm)) code)

getAmodeLit64 :: CmmLit -> NatM Amode
getAmodeLit64 lit
  = do
        let imm = litToImm lit
        tmp1    <- getNewRegNat II64
        tmp2    <- getNewRegNat II64

        let code = toOL [
                SETHI (HH imm) tmp1,
                OR False tmp1 (RIImm (HM imm)) tmp1,
                SLL tmp1 (RIImm (ImmInt 32)) tmp1,
                SETHI (LM imm) tmp2,
                OR False tmp1 (RIReg tmp2) tmp1]

        return (Amode (AddrRegImm tmp1 (LO imm)) code)
