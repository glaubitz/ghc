-- | Constants describing the DWARF format. Most of this simply
-- mirrors /usr/include/dwarf.h.

module Dwarf.Constants where

import GhcPrelude

import AsmUtils
import FastString
import Platform
import Outputable

import Reg
import qualified X86.Regs

import Data.Word

-- | Language ID used for Haskell.
dW_LANG_Haskell :: Word
dW_LANG_Haskell = 0x18
  -- Thanks to Nathan Howell for getting us our very own language ID!

-- * Dwarf tags
dW_TAG_compile_unit, dW_TAG_subroutine_type,
  dW_TAG_file_type, dW_TAG_subprogram, dW_TAG_lexical_block,
  dW_TAG_base_type, dW_TAG_structure_type, dW_TAG_pointer_type,
  dW_TAG_array_type, dW_TAG_subrange_type, dW_TAG_typedef,
  dW_TAG_variable, dW_TAG_arg_variable, dW_TAG_auto_variable,
  dW_TAG_ghc_src_note :: Word
dW_TAG_array_type      = 1
dW_TAG_lexical_block   = 11
dW_TAG_pointer_type    = 15
dW_TAG_compile_unit    = 17
dW_TAG_structure_type  = 19
dW_TAG_typedef         = 22
dW_TAG_subroutine_type = 32
dW_TAG_subrange_type   = 33
dW_TAG_base_type       = 36
dW_TAG_file_type       = 41
dW_TAG_subprogram      = 46
dW_TAG_variable        = 52
dW_TAG_auto_variable   = 256
dW_TAG_arg_variable    = 257

dW_TAG_ghc_src_note    = 0x5b00

-- * Dwarf attributes
dW_AT_name, dW_AT_stmt_list, dW_AT_low_pc, dW_AT_high_pc, dW_AT_language,
  dW_AT_comp_dir, dW_AT_producer, dW_AT_external, dW_AT_frame_base,
  dW_AT_use_UTF8, dW_AT_MIPS_linkage_name :: Word
dW_AT_name              = 0x03
dW_AT_stmt_list         = 0x10
dW_AT_low_pc            = 0x11
dW_AT_high_pc           = 0x12
dW_AT_language          = 0x13
dW_AT_comp_dir          = 0x1b
dW_AT_producer          = 0x25
dW_AT_external          = 0x3f
dW_AT_frame_base        = 0x40
dW_AT_use_UTF8          = 0x53
dW_AT_MIPS_linkage_name = 0x2007

-- * Custom DWARF attributes
-- Chosen a more or less random section of the vendor-extensible region

-- ** Describing C-- blocks
-- These appear in DW_TAG_lexical_scope DIEs corresponding to C-- blocks
dW_AT_ghc_tick_parent :: Word
dW_AT_ghc_tick_parent     = 0x2b20

-- ** Describing source notes
-- These appear in DW_TAG_ghc_src_note DIEs
dW_AT_ghc_span_file, dW_AT_ghc_span_start_line,
  dW_AT_ghc_span_start_col, dW_AT_ghc_span_end_line,
  dW_AT_ghc_span_end_col :: Word
dW_AT_ghc_span_file       = 0x2b00
dW_AT_ghc_span_start_line = 0x2b01
dW_AT_ghc_span_start_col  = 0x2b02
dW_AT_ghc_span_end_line   = 0x2b03
dW_AT_ghc_span_end_col    = 0x2b04


-- * Abbrev declarations
dW_CHILDREN_no, dW_CHILDREN_yes :: Word8
dW_CHILDREN_no  = 0
dW_CHILDREN_yes = 1

dW_FORM_addr, dW_FORM_data2, dW_FORM_data4, dW_FORM_string, dW_FORM_flag,
  dW_FORM_block1, dW_FORM_ref4, dW_FORM_ref_addr, dW_FORM_flag_present :: Word
dW_FORM_addr   = 0x01
dW_FORM_data2  = 0x05
dW_FORM_data4  = 0x06
dW_FORM_string = 0x08
dW_FORM_flag   = 0x0c
dW_FORM_block1 = 0x0a
dW_FORM_ref_addr     = 0x10
dW_FORM_ref4         = 0x13
dW_FORM_flag_present = 0x19

-- * Dwarf native types
dW_ATE_address, dW_ATE_boolean, dW_ATE_float, dW_ATE_signed,
  dW_ATE_signed_char, dW_ATE_unsigned, dW_ATE_unsigned_char :: Word
dW_ATE_address       = 1
dW_ATE_boolean       = 2
dW_ATE_float         = 4
dW_ATE_signed        = 5
dW_ATE_signed_char   = 6
dW_ATE_unsigned      = 7
dW_ATE_unsigned_char = 8

-- * Call frame information
dW_CFA_set_loc, dW_CFA_undefined, dW_CFA_same_value,
  dW_CFA_def_cfa, dW_CFA_def_cfa_offset, dW_CFA_def_cfa_expression,
  dW_CFA_expression, dW_CFA_offset_extended_sf, dW_CFA_def_cfa_offset_sf,
  dW_CFA_def_cfa_sf, dW_CFA_val_offset, dW_CFA_val_expression,
  dW_CFA_offset :: Word8
dW_CFA_set_loc            = 0x01
dW_CFA_undefined          = 0x07
dW_CFA_same_value         = 0x08
dW_CFA_def_cfa            = 0x0c
dW_CFA_def_cfa_offset     = 0x0e
dW_CFA_def_cfa_expression = 0x0f
dW_CFA_expression         = 0x10
dW_CFA_offset_extended_sf = 0x11
dW_CFA_def_cfa_sf         = 0x12
dW_CFA_def_cfa_offset_sf  = 0x13
dW_CFA_val_offset         = 0x14
dW_CFA_val_expression     = 0x16
dW_CFA_offset             = 0x80

-- * Operations
dW_OP_addr, dW_OP_deref, dW_OP_consts,
  dW_OP_minus, dW_OP_mul, dW_OP_plus,
  dW_OP_lit0, dW_OP_breg0, dW_OP_call_frame_cfa :: Word8
dW_OP_addr           = 0x03
dW_OP_deref          = 0x06
dW_OP_consts         = 0x11
dW_OP_minus          = 0x1c
dW_OP_mul            = 0x1e
dW_OP_plus           = 0x22
dW_OP_lit0           = 0x30
dW_OP_breg0          = 0x70
dW_OP_call_frame_cfa = 0x9c

-- * Dwarf section declarations
dwarfInfoSection, dwarfAbbrevSection, dwarfLineSection,
  dwarfFrameSection, dwarfGhcSection, dwarfARangesSection :: SDoc
dwarfInfoSection    = dwarfSection "info"
dwarfAbbrevSection  = dwarfSection "abbrev"
dwarfLineSection    = dwarfSection "line"
dwarfFrameSection   = dwarfSection "frame"
dwarfGhcSection     = dwarfSection "ghc"
dwarfARangesSection = dwarfSection "aranges"

dwarfSection :: String -> SDoc
dwarfSection name = sdocWithPlatform $ \plat ->
  case platformOS plat of
    os | osElfTarget os
       -> text "\t.section .debug_" <> text name <> text ",\"\","
          <> sectionType "progbits"
       | osMachOTarget os
       -> text "\t.section __DWARF,__debug_" <> text name <> text ",regular,debug"
       | otherwise
       -> text "\t.section .debug_" <> text name <> text ",\"dr\""

-- * Dwarf section labels
dwarfInfoLabel, dwarfAbbrevLabel, dwarfLineLabel, dwarfFrameLabel :: LitString
dwarfInfoLabel   = sLit ".Lsection_info"
dwarfAbbrevLabel = sLit ".Lsection_abbrev"
dwarfLineLabel   = sLit ".Lsection_line"
dwarfFrameLabel  = sLit ".Lsection_frame"

-- | Mapping of registers to DWARF register numbers
dwarfRegNo :: Platform -> Reg -> Word8
dwarfRegNo p r = case platformArch p of
  ArchX86
    | r == X86.Regs.eax  -> 0
    | r == X86.Regs.ecx  -> 1  -- yes, no typo
    | r == X86.Regs.edx  -> 2
    | r == X86.Regs.ebx  -> 3
    | r == X86.Regs.esp  -> 4
    | r == X86.Regs.ebp  -> 5
    | r == X86.Regs.esi  -> 6
    | r == X86.Regs.edi  -> 7
  ArchX86_64
    | r == X86.Regs.rax  -> 0
    | r == X86.Regs.rdx  -> 1 -- this neither. The order GCC allocates registers in?
    | r == X86.Regs.rcx  -> 2
    | r == X86.Regs.rbx  -> 3
    | r == X86.Regs.rsi  -> 4
    | r == X86.Regs.rdi  -> 5
    | r == X86.Regs.rbp  -> 6
    | r == X86.Regs.rsp  -> 7
    | r == X86.Regs.r8   -> 8
    | r == X86.Regs.r9   -> 9
    | r == X86.Regs.r10  -> 10
    | r == X86.Regs.r11  -> 11
    | r == X86.Regs.r12  -> 12
    | r == X86.Regs.r13  -> 13
    | r == X86.Regs.r14  -> 14
    | r == X86.Regs.r15  -> 15
    | r == X86.Regs.xmm0 -> 17
    | r == X86.Regs.xmm1 -> 18
    | r == X86.Regs.xmm2 -> 19
    | r == X86.Regs.xmm3 -> 20
    | r == X86.Regs.xmm4 -> 21
    | r == X86.Regs.xmm5 -> 22
    | r == X86.Regs.xmm6 -> 23
    | r == X86.Regs.xmm7 -> 24
    | r == X86.Regs.xmm8 -> 25
    | r == X86.Regs.xmm9 -> 26
    | r == X86.Regs.xmm10 -> 27
    | r == X86.Regs.xmm11 -> 28
    | r == X86.Regs.xmm12 -> 29
    | r == X86.Regs.xmm13 -> 30
    | r == X86.Regs.xmm14 -> 31
    | r == X86.Regs.xmm15 -> 32
  ArchSPARC
    | RegReal (RealRegSingle r1)  <- r             -> r1
    | RegReal (RealRegPair r1 r2) <- r, r1+1 == r2 -> 72 + r1 `shiftR` 1
  ArchSPARC64
    | RegReal (RealRegSingle r1)  <- r             -> r1
    | RegReal (RealRegPair r1 r2) <- r, r1+1 == r2 -> 72 + r1 `shiftR` 1
  _other -> error "dwarfRegNo: Unsupported platform or unknown register!"

-- | Virtual register number to use for return address.
dwarfReturnRegNo :: Platform -> Word8
dwarfReturnRegNo p
  -- We "overwrite" IP with our pseudo register - that makes sense, as
  -- when using this mechanism gdb already knows the IP anyway. Clang
  -- does this too, so it must be safe.
  = case platformArch p of
    ArchX86     -> 8  -- eip
    ArchX86_64  -> 16 -- rip
    ArchSPARC   -> 15 -- o7
    ArchSPARC64 -> 15 -- o7
    _other      -> error "dwarfReturnRegNo: Unsupported platform!"
