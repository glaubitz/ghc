{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Pretty-printing assembly language
--
-- (c) The University of Glasgow 1993-2005
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SPARC.Ppr (
        pprNatCmmDecl,
        pprBasicBlock,
        pprData,
        pprInstr,
        pprFormat,
        pprImm,
        pprDataItem
)

where

#include "HsVersions.h"
#include "nativeGen/NCG.h"

import GhcPrelude

import SPARC.Regs
import SPARC.Instr
import SPARC.Cond
import SPARC.Imm
import SPARC.AddrMode
import SPARC.Base
import Instruction
import Reg
import Format
import PprBase

import Cmm hiding (topInfoTable)
import PprCmm()
import BlockId
import CLabel
import Hoopl.Label
import Hoopl.Collections

import Unique           ( pprUniqueAlways )
import Outputable
import Platform
import FastString
import Data.Word
import Data.List

-- -----------------------------------------------------------------------------
-- Printing this stuff out

pprNatCmmDecl :: NatCmmDecl CmmStatics Instr -> SDoc
pprNatCmmDecl (CmmData section dats) =
  pprSectionAlign section $$ pprDatas dats

pprNatCmmDecl proc@(CmmProc top_info lbl _ (ListGraph blocks)) =
  case topInfoTable proc of
    Nothing ->
       case blocks of
         []     -> -- special case for split markers:
           pprLabel lbl
         blocks -> -- special case for code without info table:
           pprSectionAlign (Section Text lbl) $$
           pprLabel lbl $$ -- blocks guaranteed not null, so label needed
           vcat (map (pprBasicBlock top_info) blocks)

    Just (Statics info_lbl _) ->
      sdocWithPlatform $ \platform ->
      (if platformHasSubsectionsViaSymbols platform
          then pprSectionAlign dspSection $$
               ppr (mkDeadStripPreventer info_lbl) <> char ':'
          else empty) $$
      vcat (map (pprBasicBlock top_info) blocks) $$
      -- above: Even the first block gets a label, because with branch-chain
      -- elimination, it might be the target of a goto.
      (if platformHasSubsectionsViaSymbols platform
       then
       -- See Note [Subsections Via Symbols] in X86/Ppr.hs
                text "\t.long "
            <+> ppr info_lbl
            <+> char '-'
            <+> ppr (mkDeadStripPreventer info_lbl)
       else empty)

dspSection :: Section
dspSection = Section Text $
    panic "subsections-via-symbols doesn't combine with split-sections"

pprBasicBlock :: LabelMap CmmStatics -> NatBasicBlock Instr -> SDoc
pprBasicBlock info_env (BasicBlock blockid instrs)
  = maybe_infotable $$
    pprLabel asmLbl $$
    vcat (map pprInstr instrs)
  where
    asmLbl = blockLbl blockid
    maybe_infotable = case mapLookup blockid info_env of
       Nothing   -> empty
       Just (Statics info_lbl info) ->
           pprSectionCustomAlign (Section Text info_lbl) ReadOnlyData $$
           vcat (map pprData info) $$
           pprAlignForSection Text $$
           pprLabel info_lbl


pprDatas :: CmmStatics -> SDoc
pprDatas (Statics lbl dats) = vcat (pprLabel lbl : map pprData dats)

pprData :: CmmStatic -> SDoc
pprData (CmmString str)          = pprASCII str
pprData (CmmUninitialised bytes) = text ".skip " <> int bytes
pprData (CmmStaticLit lit)       = pprDataItem lit

pprGloblDecl :: CLabel -> SDoc
pprGloblDecl lbl
  | not (externallyVisibleCLabel lbl) = empty
  | otherwise = text ".global " <> ppr lbl

pprTypeAndSizeDecl :: CLabel -> SDoc
pprTypeAndSizeDecl lbl
    = sdocWithPlatform $ \platform ->
      if platformOS platform == OSLinux && externallyVisibleCLabel lbl
      then text ".type " <> ppr lbl <> ptext (sLit ", @object")
      else empty

pprLabel :: CLabel -> SDoc
pprLabel lbl = pprGloblDecl lbl
            $$ pprTypeAndSizeDecl lbl
            $$ (ppr lbl <> char ':')


pprASCII :: [Word8] -> SDoc
pprASCII str
  = vcat (map do1 str) $$ do1 0
    where
       do1 :: Word8 -> SDoc
       do1 w = text "\t.byte\t" <> int (fromIntegral w)


-- -----------------------------------------------------------------------------
-- pprInstr': print an 'Instr'

instance Outputable Instr where
    ppr instr = pprInstr instr


-- | Pretty print a register.
pprReg :: Reg -> SDoc
pprReg reg
 = case reg of
        RegVirtual vr
         -> case vr of
                VirtualRegI   u -> text "%vI_"   <> pprUniqueAlways u
                VirtualRegHi  u -> text "%vHi_"  <> pprUniqueAlways u
                VirtualRegF   u -> text "%vF_"   <> pprUniqueAlways u
                VirtualRegD   u -> text "%vD_"   <> pprUniqueAlways u
                VirtualRegSSE u -> text "%vSSE_" <> pprUniqueAlways u

        RegReal rr
         -> case rr of
                RealRegSingle r1
                 -> pprReg_ofRegNo r1

                RealRegPair r1 r2
                 -> text "(" <> pprReg_ofRegNo r1
                 <> vbar     <> pprReg_ofRegNo r2
                 <> text ")"



-- | Pretty print a register name, based on this register number.
--   The definition has been unfolded so we get a jump-table in the
--   object code. This function is called quite a lot when emitting
--   the asm file..
--
pprReg_ofRegNo :: Int -> SDoc
pprReg_ofRegNo i
 = ptext
    (case i of {
         0 -> sLit "%g0";   1 -> sLit "%g1";
         2 -> sLit "%g2";   3 -> sLit "%g3";
         4 -> sLit "%g4";   5 -> sLit "%g5";
         6 -> sLit "%g6";   7 -> sLit "%g7";
         8 -> sLit "%o0";   9 -> sLit "%o1";
        10 -> sLit "%o2";  11 -> sLit "%o3";
        12 -> sLit "%o4";  13 -> sLit "%o5";
        14 -> sLit "%o6";  15 -> sLit "%o7";
        16 -> sLit "%l0";  17 -> sLit "%l1";
        18 -> sLit "%l2";  19 -> sLit "%l3";
        20 -> sLit "%l4";  21 -> sLit "%l5";
        22 -> sLit "%l6";  23 -> sLit "%l7";
        24 -> sLit "%i0";  25 -> sLit "%i1";
        26 -> sLit "%i2";  27 -> sLit "%i3";
        28 -> sLit "%i4";  29 -> sLit "%i5";
        30 -> sLit "%i6";  31 -> sLit "%i7";
        32 -> sLit "%f0";  33 -> sLit "%f1";
        34 -> sLit "%f2";  35 -> sLit "%f3";
        36 -> sLit "%f4";  37 -> sLit "%f5";
        38 -> sLit "%f6";  39 -> sLit "%f7";
        40 -> sLit "%f8";  41 -> sLit "%f9";
        42 -> sLit "%f10"; 43 -> sLit "%f11";
        44 -> sLit "%f12"; 45 -> sLit "%f13";
        46 -> sLit "%f14"; 47 -> sLit "%f15";
        48 -> sLit "%f16"; 49 -> sLit "%f17";
        50 -> sLit "%f18"; 51 -> sLit "%f19";
        52 -> sLit "%f20"; 53 -> sLit "%f21";
        54 -> sLit "%f22"; 55 -> sLit "%f23";
        56 -> sLit "%f24"; 57 -> sLit "%f25";
        58 -> sLit "%f26"; 59 -> sLit "%f27";
        60 -> sLit "%f28"; 61 -> sLit "%f29";
        62 -> sLit "%f30"; 63 -> sLit "%f31";
        _  -> sLit "very naughty sparc register" })


-- | Pretty print a format for an instruction suffix.
--      eg LD is 32bit on sparc, but LDD/LDX is 64 bit.
pprFormat :: Format -> SDoc
pprFormat x
 = sdocWithPlatform $ \platform ->
   let is32Bit = target32Bit platform
   in  ptext
        (case (is32Bit, x) of
            (_, II8)       -> sLit "ub"
            (_, II16)      -> sLit "uh"
            (True , II32)  -> sLit ""
            (False, II32)  -> sLit "uw"
            (True , II64)  -> sLit "d"
            (False, II64)  -> sLit "x"
            (_, FF32)      -> sLit ""
            (_, FF64)      -> sLit "d"
            _              -> panic "SPARC.Ppr.pprFormat: no match")


-- | Pretty print a format for a store instruction suffix.
--      eg LD is 32bit on sparc, but LDD is 64 bit.
pprStFormat :: Format -> SDoc
pprStFormat x
 = sdocWithPlatform $ \platform ->
   let is32Bit = target32Bit platform
   in  ptext
        (case (is32Bit, x) of
            (_, II8)       -> sLit "b"
            (_, II16)      -> sLit "h"
            (True , II32)  -> sLit ""
            (False, II32)  -> sLit "w"
            (True , II64)  -> sLit "d"
            (False, II64)  -> sLit "x"
            (_, FF32)      -> sLit ""
            (_, FF64)      -> sLit "d"
            _              -> panic "SPARC.Ppr.pprStFormat: no match")


-- | Pretty print a condition code.
pprCond :: Cond -> SDoc
pprCond c
 = ptext
    (case c of
        ALWAYS  -> sLit ""
        NEVER   -> sLit "n"
        GEU     -> sLit "geu"
        LU      -> sLit "lu"
        EQQ     -> sLit "e"
        GTT     -> sLit "g"
        GE      -> sLit "ge"
        GU      -> sLit "gu"
        LTT     -> sLit "l"
        LE      -> sLit "le"
        LEU     -> sLit "leu"
        NE      -> sLit "ne"
        NEG     -> sLit "neg"
        POS     -> sLit "pos"
        VC      -> sLit "vc"
        VS      -> sLit "vs")


-- | Pretty print an address mode.
pprAddr :: AddrMode -> SDoc
pprAddr am
 = case am of
        AddrRegReg r1 (RegReal (RealRegSingle 0))
         -> pprReg r1

        AddrRegReg r1 r2
         -> hcat [ pprReg r1, char '+', pprReg r2 ]

        AddrRegImm r1 (ImmInt i)
         | i == 0               -> pprReg r1
         | not (fits13Bits i)   -> largeOffsetError i
         | otherwise            -> hcat [ pprReg r1, pp_sign, int i ]
         where
                pp_sign = if i > 0 then char '+' else empty

        AddrRegImm r1 (ImmInteger i)
         | i == 0               -> pprReg r1
         | not (fits13Bits i)   -> largeOffsetError i
         | otherwise            -> hcat [ pprReg r1, pp_sign, integer i ]
         where
                pp_sign = if i > 0 then char '+' else empty

        AddrRegImm r1 imm
         -> hcat [ pprReg r1, char '+', pprImm imm ]


-- | Pretty print an immediate value.
pprImm :: Imm -> SDoc
pprImm imm
 = case imm of
        ImmInt i        -> int i
        ImmInteger i    -> integer i
        ImmCLbl l       -> ppr l
        ImmIndex l i    -> ppr l <> char '+' <> int i
        ImmLit s        -> s

        ImmConstantSum a b
         -> pprImm a <> char '+' <> pprImm b

        ImmConstantDiff a b
         -> pprImm a <> char '-' <> lparen <> pprImm b <> rparen

        LO i
         -> hcat [ text "%lo(", pprImm i, rparen ]

        HI i
         -> hcat [ text "%hi(", pprImm i, rparen ]

        LM i
         -> hcat [ text "%lm(", pprImm i, rparen ]

        HM i
         -> hcat [ text "%hm(", pprImm i, rparen ]

        HH i
         -> hcat [ text "%hh(", pprImm i, rparen ]

        -- these should have been converted to bytes and placed
        --      in the data section.
        ImmFloat _      -> text "naughty float immediate"
        ImmDouble _     -> text "naughty double immediate"


-- | Pretty print a section \/ segment header.
--      On SPARC all the data sections must be at least 8 byte aligned
--      in case we store 64-bit integers or doubles in them.
--
pprSectionAlign :: Section -> SDoc
pprSectionAlign sec@(Section seg _) = pprSectionCustomAlign sec seg

-- | Pretty print a section \/ segment header with a custom alignment.
--      On SPARC instructions only need to be 4-byte aligned, but data embedded
--      in text sections must be 8-byte aligned (see the above comment).
--
pprSectionCustomAlign :: Section -> SectionType -> SDoc
pprSectionCustomAlign sec seg =
  sdocWithPlatform $ \platform ->
    pprSectionHeader platform sec $$
    pprAlignForSection seg

-- | Print appropriate alignment for the given section type.
pprAlignForSection :: SectionType -> SDoc
pprAlignForSection seg =
    ptext (case seg of
      Text              -> sLit ".align 4"
      Data              -> sLit ".align 8"
      ReadOnlyData      -> sLit ".align 8"
      RelocatableReadOnlyData
                        -> sLit ".align 8"
      UninitialisedData -> sLit ".align 8"
      ReadOnlyData16    -> sLit ".align 16"
      -- TODO: This is copied from the ReadOnlyData case, but it can likely be
      -- made more efficient.
      CString           -> sLit ".align 8"
      OtherSection _    -> panic "PprMach.pprSectionHeader: unknown section")

-- | Pretty print a data item.
pprDataItem :: CmmLit -> SDoc
pprDataItem lit
  = sdocWithDynFlags $ \dflags ->
    vcat (ppr_item (cmmTypeFormat $ cmmLitType dflags lit) lit)
    where
        imm = litToImm lit

        ppr_item II8   _        = [text "\t.byte\t" <> pprImm imm]
        ppr_item II32  _        = [text "\t.long\t" <> pprImm imm]

        ppr_item FF32  (CmmFloat r _)
         = let bs = floatToBytes (fromRational r)
           in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

        ppr_item FF64 (CmmFloat r _)
         = let bs = doubleToBytes (fromRational r)
           in  map (\b -> text "\t.byte\t" <> pprImm (ImmInt b)) bs

        ppr_item II16  _        = [text "\t.short\t" <> pprImm imm]
        ppr_item II64  _        = [text "\t.quad\t" <> pprImm imm]
        ppr_item _ _            = panic "SPARC.Ppr.pprDataItem: no match"


-- | Pretty print an instruction.
pprInstr :: Instr -> SDoc
pprInstr instr
        = sdocWithPlatform $ \platform ->
          pprInstr' (target32Bit platform) instr


pprInstr' :: Bool -> Instr -> SDoc

-- nuke comments.
pprInstr' _ (COMMENT c)
          = text "# " <> ftext c
--        = empty

pprInstr' is32Bit (DELTA d)
        = pprInstr' is32Bit (COMMENT (mkFastString ("\tdelta = " ++ show d)))

-- Newblocks and LData should have been slurped out before producing the .s file.
pprInstr' _ (NEWBLOCK _)
        = panic "SPARC.Ppr.pprInstr': NEWBLOCK"

pprInstr' _ (LDATA _ _)
        = panic "SPARC.Ppr.pprInstr': LDATA"

-- 64 bit FP loads are expanded into individual instructions in CodeGen.Expand
--pprInstr' _ (LD FF64 _ reg)
--        | RegReal (RealRegSingle{})     <- reg
--        = panic "SPARC.Ppr: not emitting potentially misaligned LD FF64 instr"

pprInstr' _ (LD format addr reg)
        = hcat [
               text "\tld",
               pprFormat format,
               char '\t',
               lbrack,
               pprAddr addr,
               pp_rbracket_comma,
               pprReg reg
            ]

-- 64 bit FP stores are expanded into individual instructions in CodeGen.Expand
--pprInstr' _ (ST FF64 reg _)
--        | RegReal (RealRegSingle{}) <- reg
--        = panic "SPARC.Ppr: not emitting potentially misaligned ST FF64 instr"

-- no distinction is made between signed and unsigned bytes on stores for the
-- Sparc opcodes (at least I cannot see any, and gas is nagging me --SOF),
-- so we call a special-purpose pprFormat for ST..
pprInstr' _ (ST format reg addr)
        = hcat [
               text "\tst",
               pprStFormat format,
               char '\t',
               pprReg reg,
               pp_comma_lbracket,
               pprAddr addr,
               rbrack
            ]


pprInstr' is32Bit (ADD x cc reg1 ri reg2)
        | not x && not cc && riZero ri
        = hcat [ text "\tmov\t", pprReg reg1, comma, pprReg reg2 ]

        | otherwise
        = pprRegRIReg is32Bit (if x then (if is32Bit then sLit "addx" else sLit "addc") else sLit "add") cc reg1 ri reg2


pprInstr' is32Bit (SUB x cc reg1 ri reg2)
        | not x && cc && reg2 == g0
        = hcat [ text "\tcmp\t", pprReg reg1, comma, pprRI ri ]

        | not x && not cc && riZero ri
        = hcat [ text "\tmov\t", pprReg reg1, comma, pprReg reg2 ]

        | otherwise
        = pprRegRIReg is32Bit (if x then (if is32Bit then sLit "subx" else sLit "subc") else sLit "sub") cc reg1 ri reg2

pprInstr' is32Bit (AND  b reg1 ri reg2) = pprRegRIReg is32Bit (sLit "and")  b reg1 ri reg2

pprInstr' is32Bit (ANDN b reg1 ri reg2) = pprRegRIReg is32Bit (sLit "andn") b reg1 ri reg2

pprInstr' is32Bit (OR b reg1 ri reg2)
        | not b && reg1 == g0
        = let doit = hcat [ text "\tmov\t", pprRI ri, comma, pprReg reg2 ]
          in  case ri of
                   RIReg rrr | rrr == reg2 -> empty
                   _                       -> doit

        | otherwise
        = pprRegRIReg is32Bit (sLit "or") b reg1 ri reg2

pprInstr' is32Bit (ORN b reg1 ri reg2)  = pprRegRIReg is32Bit (sLit "orn") b reg1 ri reg2

pprInstr' is32Bit (XOR  b reg1 ri reg2) = pprRegRIReg is32Bit (sLit "xor")  b reg1 ri reg2
pprInstr' is32Bit (XNOR b reg1 ri reg2) = pprRegRIReg is32Bit (sLit "xnor") b reg1 ri reg2

pprInstr' is32Bit (SLL reg1 ri reg2)    = pprRegRIReg is32Bit (if is32Bit then sLit "sll" else sLit "sllx") False reg1 ri reg2
pprInstr' is32Bit (SRL reg1 ri reg2)    = pprRegRIReg is32Bit (if is32Bit then sLit "srl" else sLit "srlx") False reg1 ri reg2
pprInstr' is32Bit (SRA reg1 ri reg2)    = pprRegRIReg is32Bit (if is32Bit then sLit "sra" else sLit "srax") False reg1 ri reg2
pprInstr' is32Bit (MOVR rcond reg1 ri reg2)
 | is32Bit   = panic "SPARC.Ppr: not emitting non-exitent MOVR instruction for pre-SPARCV9"
 | otherwise = pprRegRIReg is32Bit (sLit instr) False reg1 ri reg2
 where instr = case rcond of
                    EQQ -> "movrz"
                    LE  -> "movrlez"
                    LTT -> "movrlz"
                    NE  -> "movrnz"
                    GTT -> "movrgz"
                    GE  -> "movrgez"
                    _   -> panic ("SPARC.Ppr invalid condition for MOVR: " ++ (show rcond))

pprInstr' is32Bit (RDY rd)
 | is32Bit   = text "\trd\t%y," <> pprReg rd
 | otherwise = panic "SPARC.Ppr: not emitting deprecated RDY instruction for SPARCV9"

pprInstr' is32Bit (WRY reg1 reg2)
 | is32Bit   = text "\twr\t"
                       <> pprReg reg1
                       <> char ','
                       <> pprReg reg2
                       <> char ','
                       <> text "%y"
 | otherwise = panic "SPARC.Ppr: not emitting deprecated WRY instruction for SPARCV9"

pprInstr' is32Bit (SMUL b reg1 ri reg2)
 | is32Bit   = pprRegRIReg is32Bit (sLit "smul") b reg1 ri reg2
 | otherwise = panic "SPARC.Ppr: not emitting deprecated SMUL instruction for SPARCV9"

pprInstr' is32Bit (UMUL b reg1 ri reg2)
 | is32Bit   = pprRegRIReg is32Bit (sLit "umul") b reg1 ri reg2
 | otherwise = panic "SPARC.Ppr: not emitting deprecated UMUL instruction for SPARCV9"

pprInstr' is32Bit (MULX reg1 ri reg2)
 | is32Bit   = panic "SPARC.Ppr: not emitting non-existent MULX instruction for pre-SPARCV9"
 | otherwise = pprRegRIReg is32Bit (sLit "mulx") False reg1 ri reg2

pprInstr' is32Bit (SDIV b reg1 ri reg2)
 | is32Bit   = pprRegRIReg is32Bit (sLit "sdiv") b reg1 ri reg2
 | otherwise = panic "SPARC.Ppr: not emitting deprecated SDIV instruction for SPARCV9"

pprInstr' is32Bit (UDIV b reg1 ri reg2)
 | is32Bit   = pprRegRIReg is32Bit (sLit "udiv") b reg1 ri reg2
 | otherwise = panic "SPARC.Ppr: not emitting deprecated UDIV instruction for SPARCV9"

pprInstr' is32Bit (SDIVX reg1 ri reg2)
 | is32Bit   = panic "SPARC.Ppr: not emitting non-existent SDIVX instruction for pre-SPARCV9"
 | otherwise = pprRegRIReg is32Bit (sLit "sdivx") False reg1 ri reg2

pprInstr' is32Bit (UDIVX reg1 ri reg2)
 | is32Bit   = panic "SPARC.Ppr: not emitting non-existent UDIVX instruction for pre-SPARCV9"
 | otherwise = pprRegRIReg is32Bit (sLit "udivx") False reg1 ri reg2

pprInstr' _ (SETHI imm reg)
  = hcat [
        text "\tsethi\t",
        pprImm imm,
        comma,
        pprReg reg
    ]

pprInstr' _ NOP
        = text "\tnop"

pprInstr' _ (FABS format reg1 reg2)
        = pprFormatRegReg (sLit "fabs") format reg1 reg2

pprInstr' _ (FADD format reg1 reg2 reg3)
        = pprFormatRegRegReg (sLit "fadd") format reg1 reg2 reg3

pprInstr' _ (FCMP e format reg1 reg2)
        = pprFormatRegReg (if e then sLit "fcmpe" else sLit "fcmp")
                          format reg1 reg2

pprInstr' _ (FDIV format reg1 reg2 reg3)
        = pprFormatRegRegReg (sLit "fdiv") format reg1 reg2 reg3

pprInstr' _ (FMOV format reg1 reg2)
        = pprFormatRegReg (sLit "fmov") format reg1 reg2

pprInstr' _ (FMUL format reg1 reg2 reg3)
        = pprFormatRegRegReg (sLit "fmul") format reg1 reg2 reg3

pprInstr' _ (FNEG format reg1 reg2)
        = pprFormatRegReg (sLit "fneg") format reg1 reg2

pprInstr' _ (FSQRT format reg1 reg2)
        = pprFormatRegReg (sLit "fsqrt") format reg1 reg2

pprInstr' _ (FSUB format reg1 reg2 reg3)
        = pprFormatRegRegReg (sLit "fsub") format reg1 reg2 reg3

pprInstr' _ (FxTOy format1 format2 reg1 reg2)
  = hcat [
        text "\tf",
        ptext
        (case format1 of
            II32  -> sLit "ito"
            II64  -> sLit "xto"
            FF32  -> sLit "sto"
            FF64  -> sLit "dto"
            _     -> panic "SPARC.Ppr.pprInstr'.FxTOy: no match"),
        ptext
        (case format2 of
            II32  -> sLit "i\t"
            II64  -> sLit "x\t"
            FF32  -> sLit "s\t"
            FF64  -> sLit "d\t"
            _     -> panic "SPARC.Ppr.pprInstr'.FxTOy: no match"),
        pprReg reg1, comma, pprReg reg2
    ]


-- For V9, we have to use BPcc so we can use the 64-bit integer condition
-- codes. Unfortunately, this only has a 19-bit signed immediate (Bcc has a
-- 22-bit immediate), which is not enough. So, as a hack/trick, each
-- conditional branch is encoded as two branches; a conditional branch with the
-- annul bit set, and an unconditional branch. For example, to encode what
-- would normally be "ble %xcc, bar":
--
--     ble,a %xcc, .+8 # If true, branches to nop and executes delay slot
--                     # Otherwise, does not execute delay slot and continues to nop
--      b bar          # Perform the actual branch (if not annulled)
--      nop            # Acts like the delay slot for the second branch, but
--                     # only because the first branch branches here.
--
-- This nop is not emmitted; it behaves like a normal branch delay slot (albeit
-- with weird PC/nPC values), so whatever is the next instruction should go
-- there (though it will most likely be a nop).
--
-- This trick doesn't work for unconditional branches, since the annul bit
-- controls whether the delay slot is executed or not, regardless of whether
-- it's a branch always or a branch never (in fact, the behaviour of branch
-- never is consistent with conditional branches). However, these can simply be
-- encoded as the equivalent Bcc unconditional branch, which is an obvious
-- optimisation anyway.
--
-- This also does not work if the annul bit is set for the original BI, but
-- nobody should be doing that.
--
pprInstr' is32Bit (BI cond b blockid)
 | is32Bit || condUnconditional cond
     = hcat [
           text "\tb", pprCond cond,
           if b then pp_comma_a else empty,
           char '\t',
           ppr (blockLbl blockid)
       ]
 | b = panic "SPARC.Ppr.pprInstr'.BI: conditional branch with annul bit not yet implemented for V9"
 | otherwise
     = vcat $ map hcat [
                      [ text "\tb", pprCond cond, char '\t', text "%xcc", comma, text ".+8" ],
                      [ text "\t b", char '\t', ppr (blockLbl blockid) ]
                  ]


-- Technically, FBfcc is deprecated, and we should be using FBPfcc on V9 like
-- with integer condition branches. However, that will similarly reduce the
-- immediate from 22-bit to 19-bit, and I don't want more delay-slot trickery
-- than is needed.
pprInstr' _ (BF cond b blockid)
  = hcat [
        text "\tfb", pprCond cond,
        if b then pp_comma_a else empty,
        char '\t',
        ppr (blockLbl blockid)
    ]

pprInstr' _ (JMP addr) = text "\tjmp\t" <> pprAddr addr
pprInstr' is32Bit (JMP_TBL op _ _)  = pprInstr' is32Bit (JMP op)

pprInstr' _ (CALL (Left imm) params _)
  = hcat [ text "\tcall\t", pprImm imm, comma, int (length params) ]

pprInstr' _ (CALL (Right reg) params _)
  = hcat [ text "\tcall\t", pprReg reg, comma, int (length params) ]

pprInstr' _ (MEMBAR tags)
  = hcat [ text "\tmembar\t", pprMembarTags tags ]

pprInstr' _ (REGISTER reg usage)
  = hcat [ text "\t.register\t", pprReg reg, comma, pprSparcRegUsage usage ]


-- | Pretty print a tag for a membar instrution
pprMembarTag :: MembarTag -> SDoc
pprMembarTag MTLoadLoad   = text "#LoadLoad"
pprMembarTag MTStoreLoad  = text "#StoreLoad"
pprMembarTag MTLoadStore  = text "#LoadStore"
pprMembarTag MTStoreStore = text "#StoreStore"
pprMembarTag MTLookaside  = text "#Lookaside"
pprMembarTag MTMemIssue   = text "#MemIssue"
pprMembarTag MTSync       = text "#Sync"

-- | Pretty print a list of tags for a membar instruction
pprMembarTags :: [MembarTag] -> SDoc
pprMembarTags tags = hcat $ intersperse (text "|") $ map pprMembarTag tags


-- | Pretty print a .register usage
pprSparcRegUsage :: SparcRegUsage -> SDoc
pprSparcRegUsage SRUScratch = text "#scratch"
pprSparcRegUsage (SRUSym sym) = ppr sym


-- | Pretty print a RI
pprRI :: RI -> SDoc
pprRI (RIReg r) = pprReg r
pprRI (RIImm r) = pprImm r


-- | Pretty print a two reg instruction.
pprFormatRegReg :: LitString -> Format -> Reg -> Reg -> SDoc
pprFormatRegReg name format reg1 reg2
  = hcat [
        char '\t',
        ptext name,
        (case format of
            FF32 -> text "s\t"
            FF64 -> text "d\t"
            _    -> panic "SPARC.Ppr.pprFormatRegReg: no match"),

        pprReg reg1,
        comma,
        pprReg reg2
    ]


-- | Pretty print a three reg instruction.
pprFormatRegRegReg :: LitString -> Format -> Reg -> Reg -> Reg -> SDoc
pprFormatRegRegReg name format reg1 reg2 reg3
  = hcat [
        char '\t',
        ptext name,
        (case format of
            FF32  -> text "s\t"
            FF64  -> text "d\t"
            _    -> panic "SPARC.Ppr.pprFormatRegReg: no match"),
        pprReg reg1,
        comma,
        pprReg reg2,
        comma,
        pprReg reg3
    ]


-- | Pretty print an instruction of two regs and a ri.
pprRegRIReg :: Bool -> LitString -> Bool -> Reg -> RI -> Reg -> SDoc
pprRegRIReg is32Bit name b reg1 ri reg2
  = hcat [
        char '\t',
        ptext name,
        if b then (if is32Bit then text "cc\t" else text "xcc\t") else char '\t',
        pprReg reg1,
        comma,
        pprRI ri,
        comma,
        pprReg reg2
    ]

{-
pprRIReg :: Bool -> LitString -> Bool -> RI -> Reg -> SDoc
pprRIReg name b ri reg1
  = hcat [
        char '\t',
        ptext name,
        if b then if (is32Bit text "cc\t" else text "xcc\t") else char '\t',
        pprRI ri,
        comma,
        pprReg reg1
    ]
-}

{-
pp_ld_lbracket :: SDoc
pp_ld_lbracket    = text "\tld\t["
-}

pp_rbracket_comma :: SDoc
pp_rbracket_comma = text "],"


pp_comma_lbracket :: SDoc
pp_comma_lbracket = text ",["


pp_comma_a :: SDoc
pp_comma_a        = text ",a"

