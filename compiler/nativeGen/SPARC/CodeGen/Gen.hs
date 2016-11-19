-- | Evaluation of word-sized values.
module SPARC.CodeGen.Gen32 (
        getSomeReg,
        getRegister
)

where

import SPARC.CodeGen.Gen32
import SPARC.CodeGen.Gen64On32
import SPARC.CodeGen.Gen64
import SPARC.CodeGen.Base
import SPARC.Base
import NCGMonad
import Reg

import Cmm

import DynFlags

-- | The dual to getAnyReg: compute an expression into a register, but
--      we don't mind which one it is.
getSomeReg :: CmmExpr -> NatM (Reg, InstrBlock)
getSomeReg expr
  = do dflags <- getDynFlags
       let platform = targetPlatform dflags
           is32Bit  = is32BitPlatform platform
       in  if   is32Bit
           then getSomeReg32 expr
           else getSomeReg64 expr

-- | Make code to evaluate a word-sized expression.
--
getRegister :: CmmExpr -> NatM Register

getRegister expr = do
  = do dflags <- getDynFlags
       let platform = targetPlatform dflags
           is32Bit  = is32BitPlatform platform
       in  if   is32Bit
           then getRegister32 expr
           else getRegister64 expr
