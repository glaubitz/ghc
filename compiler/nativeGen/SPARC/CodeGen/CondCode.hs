module SPARC.CodeGen.CondCode (
        getCondCode,
        condIntCode,
        condFltCode
)

where

import {-# SOURCE #-} SPARC.CodeGen.Gen
import SPARC.CodeGen.Base
import SPARC.Instr
import SPARC.Regs
import SPARC.Cond
import SPARC.Imm
import SPARC.Base
import NCGMonad
import Format

import Cmm

import OrdList
import Outputable
import Platform

getCondCode :: CmmExpr -> NatM CondCode
getCondCode (CmmMachOp mop [x, y]) = do
    dflags <- getDynFlags
    case mop of
      MO_F_Eq W32 -> condFltCode EQQ x y
      MO_F_Ne W32 -> condFltCode NE  x y
      MO_F_Gt W32 -> condFltCode GTT x y
      MO_F_Ge W32 -> condFltCode GE  x y
      MO_F_Lt W32 -> condFltCode LTT x y
      MO_F_Le W32 -> condFltCode LE  x y

      MO_F_Eq W64 -> condFltCode EQQ x y
      MO_F_Ne W64 -> condFltCode NE  x y
      MO_F_Gt W64 -> condFltCode GTT x y
      MO_F_Ge W64 -> condFltCode GE  x y
      MO_F_Lt W64 -> condFltCode LTT x y
      MO_F_Le W64 -> condFltCode LE  x y

      MO_Eq rep   -> condIntCode EQQ (extendUExpr dflags rep x)
                                     (extendUExpr dflags rep y)
      MO_Ne rep   -> condIntCode NE  (extendUExpr dflags rep x)
                                     (extendUExpr dflags rep y)

      MO_S_Gt rep -> condIntCode GTT (extendSExpr dflags rep x)
                                     (extendSExpr dflags rep y)
      MO_S_Ge rep -> condIntCode GE  (extendSExpr dflags rep x)
                                     (extendSExpr dflags rep y)
      MO_S_Lt rep -> condIntCode LTT (extendSExpr dflags rep x)
                                     (extendSExpr dflags rep y)
      MO_S_Le rep -> condIntCode LE  (extendSExpr dflags rep x)
                                     (extendSExpr dflags rep y)

      MO_U_Gt rep -> condIntCode GU  (extendUExpr dflags rep x)
                                     (extendUExpr dflags rep y)
      MO_U_Ge rep -> condIntCode GEU (extendUExpr dflags rep x)
                                     (extendUExpr dflags rep y)
      MO_U_Lt rep -> condIntCode LU  (extendUExpr dflags rep x)
                                     (extendUExpr dflags rep y)
      MO_U_Le rep -> condIntCode LEU (extendUExpr dflags rep x)
                                     (extendUExpr dflags rep y)

      _           -> pprPanic "SPARC.CodeGen.CondCode.getCondCode" (ppr (CmmMachOp mop [x,y]))

getCondCode other = pprPanic "SPARC.CodeGen.CondCode.getCondCode" (ppr other)





-- @cond(Int|Flt)Code@: Turn a boolean expression into a condition, to be
-- passed back up the tree.

condIntCode :: Cond -> CmmExpr -> CmmExpr -> NatM CondCode
condIntCode cond x (CmmLit (CmmInt y _))
  | fits13Bits y
  = do
       (src1, code) <- getSomeReg x
       let
           src2 = ImmInt (fromInteger y)
           code' = code `snocOL` SUB False True src1 (RIImm src2) g0
       return (CondCode False cond code')

condIntCode cond x y = do
    (src1, code1) <- getSomeReg x
    (src2, code2) <- getSomeReg y
    let
        code__2 = code1 `appOL` code2 `snocOL`
                  SUB False True src1 (RIReg src2) g0
    return (CondCode False cond code__2)


condFltCode :: Cond -> CmmExpr -> CmmExpr -> NatM CondCode
condFltCode cond x y = do
    dflags <- getDynFlags
    (src1, code1) <- getSomeReg x
    (src2, code2) <- getSomeReg y
    tmp <- getNewRegNat FF64
    let
        promote x = FxTOy FF32 FF64 x tmp

        pk1   = cmmExprType dflags x
        pk2   = cmmExprType dflags y

        code__2 =
                if pk1 `cmmEqType` pk2 then
                    code1 `appOL` code2 `snocOL`
                    FCMP True (cmmTypeFormat pk1) src1 src2
                else if typeWidth pk1 == W32 then
                    code1 `snocOL` promote src1 `appOL` code2 `snocOL`
                    FCMP True FF64 tmp src2
                else
                    code1 `appOL` code2 `snocOL` promote src2 `snocOL`
                    FCMP True FF64 src1 tmp
    return (CondCode True cond code__2)
