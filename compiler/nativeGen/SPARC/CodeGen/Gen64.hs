-- | Evaluation of native 64 bit values.
module SPARC.CodeGen.Gen64 (
        getSomeReg64,
        getRegister64
)

where

import SPARC.CodeGen.CondCode
import SPARC.CodeGen.Amode
import SPARC.CodeGen.Base
import SPARC.Stack
import SPARC.Instr
import SPARC.Cond
import SPARC.AddrMode
import SPARC.Imm
import SPARC.Regs
import SPARC.Base
import NCGMonad
import Format
import Reg

import Cmm

import Control.Monad (liftM)
import DynFlags
import OrdList
import Outputable

-- | The dual to getAnyReg: compute an expression into a register, but
--      we don't mind which one it is.
getSomeReg64 :: CmmExpr -> NatM (Reg, InstrBlock)
getSomeReg64 expr = do
  r <- getRegister64 expr
  case r of
    Any rep code -> do
        tmp <- getNewRegNat rep
        return (tmp, code tmp)
    Fixed _ reg code ->
        return (reg, code)



-- | Make code to evaluate a 64 bit expression.
--
getRegister64 :: CmmExpr -> NatM Register

getRegister64 (CmmReg reg)
  = do dflags <- getDynFlags
       let platform = targetPlatform dflags
       return (Fixed (cmmTypeFormat (cmmRegType dflags reg))
                     (getRegisterReg platform reg) nilOL)

getRegister64 tree@(CmmRegOff _ _)
  = do dflags <- getDynFlags
       getRegister64 (mangleIndexTree dflags tree)

-- Load a literal float into a float register.
--      The actual literal is stored in a new data area, and we load it
--      at runtime.
getRegister64 (CmmLit (CmmFloat f frep)) = do
    lbl  <- getNewLabelNat
    dflags <- getDynFlags
    dynRef <- cmmMakeDynamicReference dflags DataReference lbl
    Amode addr addr_code <- getAmode dynRef
    let fformat = floatFormat frep
        iformat = intFormat frep
        code dst =
            LDATA (Section ReadOnlyData lbl)
                  (Statics lbl [CmmStaticLit (CmmFloat f frep)])
            `consOL` (addr_code `snocOL` LD iformat addr dst)
    return (Any fformat code)


-- Unary machine ops
getRegister64 (CmmMachOp mop [x])
  = case mop of
        -- Floating point negation -------------------------
        MO_F_Neg W32            -> trivialUFCode FF32 (FNEG FF32) x
        MO_F_Neg W64            -> trivialUFCode FF64 (FNEG FF64) x


        -- Integer negation --------------------------------
        MO_S_Neg rep            -> trivialUCode (intFormat rep) (SUB False False g0) x
        MO_Not rep              -> trivialUCode (intFormat rep) (XNOR False g0) x


        -- Float word size conversion ----------------------
        MO_FF_Conv W64 W32      -> coerceDbl2Flt x
        MO_FF_Conv W32 W64      -> coerceFlt2Dbl x


        -- Float <-> Signed Int conversion -----------------
        MO_FS_Conv from to      -> coerceFP2Int from to x
        MO_SF_Conv from to      -> coerceInt2FP from to x


        -- Unsigned integer word size conversions ----------

        -- If it's the same size, then nothing needs to be done.
        MO_UU_Conv from to
         | from == to           -> conversionNop (intFormat to)  x

        -- To narrow an unsigned word, mask out the high bits to simulate what would
        --      happen if we copied the value into a smaller register.
        MO_UU_Conv W16 W8       -> trivialCode W8  (AND False) x (CmmLit (CmmInt 255 W8))
        MO_UU_Conv W32 W8       -> trivialCode W8  (AND False) x (CmmLit (CmmInt 255 W8))
        MO_UU_Conv W64 W8       -> trivialCode W8  (AND False) x (CmmLit (CmmInt 255 W8))

        -- for narrowing to 16/32 bit, don't use a literal mask value like the 8 bit
        --      case because the only way we can load it is via SETHI, which needs 2 ops.
        --      Do some shifts to chop out the high bits instead.
        MO_UU_Conv W32 W16
         -> do  tmpReg          <- getNewRegNat II64
                (xReg, xCode)   <- getSomeReg64 x
                let code dst
                        =       xCode
                        `appOL` toOL
                                [ SLL xReg   (RIImm $ ImmInt 48) tmpReg
                                , SRL tmpReg (RIImm $ ImmInt 48) dst]

                return  $ Any II32 code
        MO_UU_Conv W64 W16
         -> do  tmpReg          <- getNewRegNat II64
                (xReg, xCode)   <- getSomeReg64 x
                let code dst
                        =       xCode
                        `appOL` toOL
                                [ SLL xReg   (RIImm $ ImmInt 48) tmpReg
                                , SRL tmpReg (RIImm $ ImmInt 48) dst]

                return  $ Any II32 code
        MO_UU_Conv W64 W32
         -> do  tmpReg          <- getNewRegNat II64
                (xReg, xCode)   <- getSomeReg64 x
                let code dst
                        =       xCode
                        `appOL` toOL
                                [ SLL xReg   (RIImm $ ImmInt 32) tmpReg
                                , SRL tmpReg (RIImm $ ImmInt 32) dst]

                return  $ Any II32 code

        -- To widen an unsigned word we don't have to do anything.
        --      Just leave it in the same register and mark the result as the new size.
        MO_UU_Conv W8  W16      -> conversionNop (intFormat W16)  x
        MO_UU_Conv W8  W32      -> conversionNop (intFormat W32)  x
        MO_UU_Conv W8  W64      -> conversionNop (intFormat W64)  x
        MO_UU_Conv W16 W32      -> conversionNop (intFormat W32)  x
        MO_UU_Conv W16 W64      -> conversionNop (intFormat W64)  x
        MO_UU_Conv W32 W64      -> conversionNop (intFormat W64)  x


        -- Signed integer word size conversions ------------

        -- Mask out high bits when narrowing them
        MO_SS_Conv W16 W8       -> trivialCode W8  (AND False) x (CmmLit (CmmInt 255 W8))
        MO_SS_Conv W32 W8       -> trivialCode W8  (AND False) x (CmmLit (CmmInt 255 W8))
        MO_SS_Conv W64 W8       -> trivialCode W8  (AND False) x (CmmLit (CmmInt 255 W8))
        -- same optimisation as unsigned case
        MO_SS_Conv W32 W16
         -> do  tmpReg          <- getNewRegNat II64
                (xReg, xCode)   <- getSomeReg64 x
                let code dst
                        =       xCode
                        `appOL` toOL
                                [ SLL xReg   (RIImm $ ImmInt 48) tmpReg
                                , SRL tmpReg (RIImm $ ImmInt 48) dst]

                return  $ Any II32 code
        MO_SS_Conv W64 W16
         -> do  tmpReg          <- getNewRegNat II64
                (xReg, xCode)   <- getSomeReg64 x
                let code dst
                        =       xCode
                        `appOL` toOL
                                [ SLL xReg   (RIImm $ ImmInt 48) tmpReg
                                , SRL tmpReg (RIImm $ ImmInt 48) dst]

                return  $ Any II32 code
        MO_SS_Conv W64 W32
         -> do  tmpReg          <- getNewRegNat II64
                (xReg, xCode)   <- getSomeReg64 x
                let code dst
                        =       xCode
                        `appOL` toOL
                                [ SLL xReg   (RIImm $ ImmInt 32) tmpReg
                                , SRL tmpReg (RIImm $ ImmInt 32) dst]

                return  $ Any II32 code

        -- Sign extend signed words when widening them.
        MO_SS_Conv W8  W16      -> integerExtend W8  W16 x
        MO_SS_Conv W8  W32      -> integerExtend W8  W32 x
        MO_SS_Conv W8  W64      -> integerExtend W8  W64 x
        MO_SS_Conv W16 W32      -> integerExtend W16 W32 x
        MO_SS_Conv W16 W64      -> integerExtend W16 W64 x
        MO_SS_Conv W32 W64      -> integerExtend W32 W64 x

        _                       -> panic ("Unknown unary mach op: " ++ show mop)


-- Binary machine ops
getRegister64 (CmmMachOp mop [x, y])
  = case mop of
      MO_Eq _           -> condIntReg EQQ x y
      MO_Ne _           -> condIntReg NE x y

      MO_S_Gt _         -> condIntReg GTT x y
      MO_S_Ge _         -> condIntReg GE x y
      MO_S_Lt _         -> condIntReg LTT x y
      MO_S_Le _         -> condIntReg LE x y

      MO_U_Gt W64       -> condIntReg GU  x y
      MO_U_Ge W64       -> condIntReg GEU x y
      MO_U_Lt W64       -> condIntReg LU  x y
      MO_U_Le W64       -> condIntReg LEU x y

      MO_U_Gt W32       -> condIntReg GU  x y
      MO_U_Ge W32       -> condIntReg GEU x y
      MO_U_Lt W32       -> condIntReg LU  x y
      MO_U_Le W32       -> condIntReg LEU x y

      MO_U_Gt W16       -> condIntReg GU  x y
      MO_U_Ge W16       -> condIntReg GEU x y
      MO_U_Lt W16       -> condIntReg LU  x y
      MO_U_Le W16       -> condIntReg LEU x y

      MO_Add W64        -> trivialCode W64 (ADD False False) x y
      MO_Sub W64        -> trivialCode W64 (SUB False False) x y

      MO_S_Quot W64     -> idiv True  x y
      MO_U_Quot W64     -> idiv False x y

      MO_S_Rem  W64     -> irem True  x y
      MO_U_Rem  W64     -> irem False x y

      MO_F_Eq _         -> condFltReg EQQ x y
      MO_F_Ne _         -> condFltReg NE x y

      MO_F_Gt _         -> condFltReg GTT x y
      MO_F_Ge _         -> condFltReg GE x y
      MO_F_Lt _         -> condFltReg LTT x y
      MO_F_Le _         -> condFltReg LE x y

      MO_F_Add  w       -> trivialFCode w FADD x y
      MO_F_Sub  w       -> trivialFCode w FSUB x y
      MO_F_Mul  w       -> trivialFCode w FMUL x y
      MO_F_Quot w       -> trivialFCode w FDIV x y

      MO_And rep        -> trivialCode rep (AND False) x y
      MO_Or  rep        -> trivialCode rep (OR  False) x y
      MO_Xor rep        -> trivialCode rep (XOR False) x y

      MO_Mul rep        -> trivialCode rep MULX x y

      MO_Shl rep        -> trivialCode rep SLL  x y
      MO_U_Shr rep      -> trivialCode rep SRL x y
      MO_S_Shr rep      -> trivialCode rep SRA x y

      _                 -> pprPanic "getRegister64(sparc) - binary CmmMachOp (1)" (pprMachOp mop)
  where


getRegister64 (CmmLoad mem pk) = do
    Amode src code <- getAmode mem
    let
        code__2 dst     = code `snocOL` LD (cmmTypeFormat pk) src dst
    return (Any (cmmTypeFormat pk) code__2)

getRegister64 (CmmLit (CmmInt i _))
  | fits13Bits i
  = let
        src = ImmInt (fromInteger i)
        code dst = unitOL (OR False g0 (RIImm src) dst)
    in
        return (Any II64 code)

-- TODO
-- getRegister64 (CmmLit (CmmInt i _))
--   | fits32Bits i
--   = let
--         src = ImmInt (fromInteger i)
--         code dst = toOL [
--             SETHI (HI (RIImm src)) dst,
--             OR False dst (RIImm (LO (RIImm src))) dst]
--     in
--         return (Any II64 code)

getRegister64 (CmmLit lit) = do
    tmp <- getNewRegNat II64
    let imm = litToImm lit
        code dst = toOL [
            SETHI (HH imm) tmp,
            SETHI (LM imm) dst,
            OR False tmp (RIImm (HM imm)) tmp,
            OR False dst (RIImm (LO imm)) dst,
            SLL tmp (RIImm (ImmInt 32)) tmp,
            OR False dst (RIReg tmp) dst]
    return (Any II64 code)


getRegister64 _
        = panic "SPARC.CodeGen.Gen64.getRegister64: no match"


-- | sign extend and widen
integerExtend
        :: Width                -- ^ width of source expression
        -> Width                -- ^ width of result
        -> CmmExpr              -- ^ source expression
        -> NatM Register

integerExtend from to expr
 = do   -- load the expr into some register
        (reg, e_code)   <- getSomeReg64 expr
        tmp             <- getNewRegNat II32
        let bitCount
                = case (from, to) of
                        (W8,  W16)      -> 56
                        (W8,  W32)      -> 56
                        (W8,  W64)      -> 56
                        (W16, W32)      -> 48
                        (W16, W64)      -> 48
                        (W32, W64)      -> 32
                        _               -> panic "SPARC.CodeGen.Gen64: no match"
        let code dst
                = e_code

                -- local shift word left to load the sign bit
                `snocOL`  SLL reg (RIImm (ImmInt bitCount)) tmp

                -- arithmetic shift right to sign extend
                `snocOL`  SRA tmp (RIImm (ImmInt bitCount)) dst

        return (Any (intFormat to) code)


-- | For nop word format conversions we set the resulting value to have the
--      required size, but don't need to generate any actual code.
--
conversionNop
        :: Format -> CmmExpr -> NatM Register

conversionNop new_rep expr
 = do   e_code <- getRegister64 expr
        return (setFormatOfRegister e_code new_rep)



-- | Generate an integer division instruction.
idiv :: Bool -> CmmExpr -> CmmExpr -> NatM Register

-- Unsigned/signed 64-bit division; no Y register here
idiv signed x y
 = do
        (a_reg, a_code)         <- getSomeReg64 x
        (b_reg, b_code)         <- getSomeReg64 y

        let code dst
                =        a_code
                `appOL`  b_code
                `snocOL` (if signed then SDIVX else UDIVX) a_reg (RIReg b_reg) dst

        return (Any II64 code)


-- | Do an integer remainder; no hardware instruction.
irem :: Bool -> CmmExpr -> CmmExpr -> NatM Register
irem signed x y
 = do
        (a_reg, a_code) <- getSomeReg64 x
        (b_reg, b_code) <- getSomeReg64 y

        tmp_reg         <- getNewRegNat II64

        let code dst
                =       a_code
                `appOL` b_code
                `appOL` toOL
                        [ (if signed then SDIVX else UDIVX) a_reg (RIReg b_reg) tmp_reg
                        , MULX tmp_reg (RIReg b_reg) tmp_reg
                        , SUB   False False   a_reg (RIReg tmp_reg) dst]

        return  (Any II64 code)


-- -----------------------------------------------------------------------------
-- 'trivial*Code': deal with trivial instructions

-- Trivial (dyadic: 'trivialCode', floating-point: 'trivialFCode',
-- unary: 'trivialUCode', unary fl-pt:'trivialUFCode') instructions.
-- Only look for constants on the right hand side, because that's
-- where the generic optimizer will have put them.

-- Similarly, for unary instructions, we don't have to worry about
-- matching an StInt as the argument, because genericOpt will already
-- have handled the constant-folding.

trivialCode
        :: Width
        -> (Reg -> RI -> Reg -> Instr)
        -> CmmExpr
        -> CmmExpr
        -> NatM Register

trivialCode _ instr x (CmmLit (CmmInt y _))
  | fits13Bits y
  = do
      (src1, code) <- getSomeReg64 x
      let
        src2 = ImmInt (fromInteger y)
        code__2 dst = code `snocOL` instr src1 (RIImm src2) dst
      return (Any II64 code__2)


trivialCode _ instr x y = do
    (src1, code1) <- getSomeReg64 x
    (src2, code2) <- getSomeReg64 y
    let
        code__2 dst = code1 `appOL` code2 `snocOL`
                      instr src1 (RIReg src2) dst
    return (Any II64 code__2)


trivialFCode
        :: Width
        -> (Format -> Reg -> Reg -> Reg -> Instr)
        -> CmmExpr
        -> CmmExpr
        -> NatM Register

trivialFCode pk instr x y = do
    dflags <- getDynFlags
    (src1, code1) <- getSomeReg64 x
    (src2, code2) <- getSomeReg64 y
    tmp <- getNewRegNat FF64
    let
        promote x = FxTOy FF32 FF64 x tmp

        pk1   = cmmExprType dflags x
        pk2   = cmmExprType dflags y

        code__2 dst =
                if pk1 `cmmEqType` pk2 then
                    code1 `appOL` code2 `snocOL`
                    instr (floatFormat pk) src1 src2 dst
                else if typeWidth pk1 == W32 then
                    code1 `snocOL` promote src1 `appOL` code2 `snocOL`
                    instr FF64 tmp src2 dst
                else
                    code1 `appOL` code2 `snocOL` promote src2 `snocOL`
                    instr FF64 src1 tmp dst
    return (Any (cmmTypeFormat $ if pk1 `cmmEqType` pk2 then pk1 else cmmFloat W64)
                code__2)



trivialUCode
        :: Format
        -> (RI -> Reg -> Instr)
        -> CmmExpr
        -> NatM Register

trivialUCode format instr x = do
    (src, code) <- getSomeReg64 x
    let
        code__2 dst = code `snocOL` instr (RIReg src) dst
    return (Any format code__2)


trivialUFCode
        :: Format
        -> (Reg -> Reg -> Instr)
        -> CmmExpr
        -> NatM Register

trivialUFCode pk instr x = do
    (src, code) <- getSomeReg64 x
    let
        code__2 dst = code `snocOL` instr src dst
    return (Any pk code__2)




-- Coercions -------------------------------------------------------------------

-- | Coerce a integer value to floating point
coerceInt2FP :: Width -> Width -> CmmExpr -> NatM Register
coerceInt2FP width1 width2 x = do
    (src, code) <- getSomeReg64 x
    let
        code__2 dst = code `appOL` toOL [
            ST (intFormat width1) src (spRel False (-2)),
            LD (intFormat width1) (spRel False (-2)) dst,
            FxTOy (intFormat width1) (floatFormat width2) dst dst]
    return (Any (floatFormat $ width2) code__2)



-- | Coerce a floating point value to integer
--
--   NOTE: On sparc v9 there are no instructions to move a value from an
--         FP register directly to an int register, so we have to use a load/store.
--
coerceFP2Int :: Width -> Width -> CmmExpr -> NatM Register
coerceFP2Int width1 width2 x
 = do   let fformat1      = floatFormat width1
            fformat2      = floatFormat width2

            iformat2      = intFormat   width2

        (fsrc, code)    <- getSomeReg64 x
        fdst            <- getNewRegNat fformat2

        let code2 dst
                =       code
                `appOL` toOL
                        -- convert float to int format, leaving it in a float reg.
                        [ FxTOy fformat1 iformat2 fsrc fdst

                        -- store the int into mem, then load it back to move
                        --      it into an actual int reg.
                        , ST    fformat2 fdst (spRel False (-2))
                        , LD    iformat2 (spRel False (-2)) dst]

        return (Any iformat2 code2)


-- | Coerce a double precision floating point value to single precision.
coerceDbl2Flt :: CmmExpr -> NatM Register
coerceDbl2Flt x = do
    (src, code) <- getSomeReg64 x
    return (Any FF32 (\dst -> code `snocOL` FxTOy FF64 FF32 src dst))


-- | Coerce a single precision floating point value to double precision
coerceFlt2Dbl :: CmmExpr -> NatM Register
coerceFlt2Dbl x = do
    (src, code) <- getSomeReg64 x
    return (Any FF64 (\dst -> code `snocOL` FxTOy FF32 FF64 src dst))




-- Condition Codes -------------------------------------------------------------
--
-- Evaluate a comparison, and get the result into a register.
--
-- Do not fill the delay slots here. you will confuse the register allocator.
--
condIntReg :: Cond -> CmmExpr -> CmmExpr -> NatM Register
condIntReg EQQ x (CmmLit (CmmInt 0 _)) = do
    (src, code) <- getSomeReg64 x
    let
        code__2 dst = code `appOL` toOL [
            SUB False True g0 (RIReg src) g0,
            SUB True False g0 (RIImm (ImmInt (-1))) dst]
    return (Any II64 code__2)

condIntReg EQQ x y = do
    (src1, code1) <- getSomeReg64 x
    (src2, code2) <- getSomeReg64 y
    let
        code__2 dst = code1 `appOL` code2 `appOL` toOL [
            XOR False src1 (RIReg src2) dst,
            SUB False True g0 (RIReg dst) g0,
            SUB True False g0 (RIImm (ImmInt (-1))) dst]
    return (Any II64 code__2)

condIntReg NE x (CmmLit (CmmInt 0 _)) = do
    (src, code) <- getSomeReg64 x
    let
        code__2 dst = code `appOL` toOL [
            SUB False True g0 (RIReg src) g0,
            ADD True False g0 (RIImm (ImmInt 0)) dst]
    return (Any II64 code__2)

condIntReg NE x y = do
    (src1, code1) <- getSomeReg64 x
    (src2, code2) <- getSomeReg64 y
    let
        code__2 dst = code1 `appOL` code2 `appOL` toOL [
            XOR False src1 (RIReg src2) dst,
            SUB False True g0 (RIReg dst) g0,
            ADD True False g0 (RIImm (ImmInt 0)) dst]
    return (Any II64 code__2)

condIntReg cond x y = do
    bid1 <- liftM (\a -> seq a a) getBlockIdNat
    bid2 <- liftM (\a -> seq a a) getBlockIdNat
    CondCode _ cond cond_code <- condIntCode cond x y
    let
        code__2 dst
         =      cond_code
          `appOL` toOL
                [ BI cond False bid1
                , NOP

                , OR False g0 (RIImm (ImmInt 0)) dst
                , BI ALWAYS False bid2
                , NOP

                , NEWBLOCK bid1
                , OR False g0 (RIImm (ImmInt 1)) dst
                , BI ALWAYS False bid2
                , NOP

                , NEWBLOCK bid2]

    return (Any II64 code__2)


condFltReg :: Cond -> CmmExpr -> CmmExpr -> NatM Register
condFltReg cond x y = do
    bid1 <- liftM (\a -> seq a a) getBlockIdNat
    bid2 <- liftM (\a -> seq a a) getBlockIdNat

    CondCode _ cond cond_code <- condFltCode cond x y
    let
        code__2 dst
         =      cond_code
          `appOL` toOL
                [ NOP
                , BF cond False bid1
                , NOP

                , OR False g0 (RIImm (ImmInt 0)) dst
                , BI ALWAYS False bid2
                , NOP

                , NEWBLOCK bid1
                , OR False g0 (RIImm (ImmInt 1)) dst
                , BI ALWAYS False bid2
                , NOP

                , NEWBLOCK bid2 ]

    return (Any II64 code__2)
