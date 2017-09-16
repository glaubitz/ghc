module SPARC.Imm (
        -- immediate values
        Imm(..),
        strImmLit,
        litToImm,
        litToDynamicLinkerLabelInfo
)

where

import GhcPrelude

import Cmm
import CLabel

import Outputable

-- | An immediate value.
--      Not all of these are directly representable by the machine.
--      Things like ImmLit are slurped out and put in a data segment instead.
--
data Imm
        = ImmInt        Int

        -- Sigh.
        | ImmInteger    Integer

        -- AbstractC Label (with baggage)
        | ImmCLbl       CLabel

        -- Simple string
        | ImmLit        SDoc
        | ImmIndex      CLabel Int
        | ImmFloat      Rational
        | ImmDouble     Rational

        | ImmConstantSum  Imm Imm
        | ImmConstantDiff Imm Imm

        -- 32-bit %lo/%hi
        | LO    Imm
        | HI    Imm

        -- 64-bit (%lo/)%lm/%hm/%hh
        | LM    Imm
        | HM    Imm
        | HH    Imm

        -- %lox/%hix needed for loading negative 32-bit constants efficiently on V9
        | LOX   Imm
        | HIX   Imm

        -- GOT offsets + load hint
        | GDOP       Imm
        | GDOP_LOX10 Imm
        | GDOP_HIX22 Imm
        deriving Show

instance Show SDoc where
    show _ = "SDoc"

instance Show CLabel where
    show _ = "<label>"


-- | Create a ImmLit containing this string.
strImmLit :: String -> Imm
strImmLit s = ImmLit (text s)


-- | Convert a CmmLit to an Imm.
--      Narrow to the width: a CmmInt might be out of
--      range, but we assume that ImmInteger only contains
--      in-range values.  A signed value should be fine here.
--
litToImm :: CmmLit -> Imm
litToImm lit
 = case lit of
        CmmInt i w              -> ImmInteger (narrowS w i)
        CmmFloat f W32          -> ImmFloat f
        CmmFloat f W64          -> ImmDouble f
        CmmLabel l              -> ImmCLbl l
        CmmLabelOff l off       -> ImmIndex l off

        CmmLabelDiffOff l1 l2 off
         -> ImmConstantSum
                (ImmConstantDiff (ImmCLbl l1) (ImmCLbl l2))
                (ImmInt off)

        _               -> panic "SPARC.Regs.litToImm: no match"

litToDynamicLinkerLabelInfo :: CmmLit -> Maybe (DynamicLinkerLabelInfo, CLabel)
litToDynamicLinkerLabelInfo lit
 = case lit of
        CmmInt _ _              -> Nothing
        CmmFloat _ _            -> Nothing
        CmmLabel l              -> dynamicLinkerLabelInfo l
        CmmLabelOff l _         -> dynamicLinkerLabelInfo l
        CmmLabelDiffOff _ _ _   -> Nothing

        _                       -> panic "SPARC.Regs.litToDynamicLinkerLabelInfo: no match"
