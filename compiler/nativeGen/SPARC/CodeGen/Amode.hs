module SPARC.CodeGen.Amode (
        getAmode
)

where

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
                SETHI (HI imm) tmp1,
                OR False tmp1 (RIImm (HM imm)) tmp1,
                SLL tmp1 (RIImm (ImmInt 32)) tmp1,
                SETHI (LM imm) tmp2,
                OR False tmp1 (RIReg tmp2) tmp1]

        return (Amode (AddrRegImm tmp1 (LO imm)) code)
