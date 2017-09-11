module SPARC.Stack (
        stackBias,
        paramArrayStartSlot,
        requiredParamArraySlots,
        spRel,
        spRel2,
        fpRel,
        fpRel2,
        spillSlotToOffset,
        maxSpillSlots
)

where

import GhcPrelude

import SPARC.AddrMode
import SPARC.Regs
import SPARC.Base
import SPARC.Imm
import Platform

import DynFlags
import Outputable

stackBias :: Bool -> Int

stackBias is32Bit
 | is32Bit   = 0
 | otherwise = 2047

-- | Get the number of slots in the register save area
saveAreaSlots :: Int
saveAreaSlots = 16

-- | Get the size of the register save area
saveAreaBytes :: Bool -> Int
saveAreaBytes is32Bit = saveAreaSlots * (wordLength is32Bit)

-- | Get the number of slots used for the struct/union return pointer
structRetPtrSlots :: Bool -> Int
structRetPtrSlots is32Bit
 | is32Bit   = 1
 | otherwise = 0

-- | Get the number of bytes used for the struct/union return pointer
structRetPtrBytes :: Bool -> Int
structRetPtrBytes is32Bit = (structRetPtrSlots is32Bit) * (wordLength is32Bit)

-- | Gets the starting slot of the parameter array
paramArrayStartSlot :: Bool -> Int
paramArrayStartSlot is32Bit = saveAreaSlots + (structRetPtrSlots is32Bit)

-- | Get the minimum number of slots required in the parameter array
requiredParamArraySlots :: Int
requiredParamArraySlots = 6

-- | Get the size of the required parameter array
requiredParamArrayBytes :: Bool -> Int
requiredParamArrayBytes is32Bit = requiredParamArraySlots * (wordLength is32Bit)

-- | Get an AddrMode relative to the address in sp.
--      This gives us a stack relative addressing mode for volatile
--      temporaries and for excess call arguments.
--
spRel :: Bool
      -> Int            -- ^ stack offset in words, positive or negative
      -> AddrMode
spRel is32Bit n = spRel2 is32Bit n 0

spRel2 :: Bool
       -> Int            -- ^ stack offset in words, positive or negative
       -> Int            -- ^ additional offset in bytes, positive or negative
       -> AddrMode
spRel2 = xpRel2 sp


-- | Get an address relative to the frame pointer.
--      This doesn't work work for offsets greater than 13 bits; we just hope for the best
--
fpRel :: Bool -> Int -> AddrMode
fpRel is32Bit n = fpRel2 is32Bit n 0

fpRel2 :: Bool -> Int -> Int -> AddrMode
fpRel2 = xpRel2 fp


xpRel2 :: Reg -> Bool -> Int -> Int -> AddrMode
xpRel2 xp is32Bit n off
        | fits13Bits imm
        = AddrRegImm xp (ImmInt imm)
        | otherwise
        = panic "SPARC.Stack.xpRel2: need far load/store"
        where imm = ((stackBias is32Bit) + n * (wordLength is32Bit) + off)

-- | Convert a spill slot number to a *byte* offset *below %fp+BIAS*, with no sign.
--
spillSlotToOffset :: DynFlags -> Int -> Int
spillSlotToOffset dflags slot
        | slot >= 0 && slot < maxSpillSlots dflags
        = spillSlotSize * slot

        | otherwise
        = pprPanic "spillSlotToOffset:"
                      (   text "invalid spill location: " <> int slot
                      $$  text "maxSpillSlots:          " <> int (maxSpillSlots dflags))


-- | The maximum number of spill slots available on the C stack.
--      If we use up all of the slots, then we're screwed.
--
--      Why do we reserve 64 bytes, instead of using the whole thing??
--              -- BL 2009/02/15
--
--      We actually need to reserve more than 64 bytes, even for 32-bit.
--      The bottom 16 words are required to be used as the window save area;
--      32-bit then has a struct/union return pointer entry (passed as a
--      normal arg when required on 64-bit), but both then have a 6 word
--      parameter save area.
--              -- JC 2017/07/22
--
maxSpillSlots :: DynFlags -> Int
maxSpillSlots dflags
        = ((spillAreaLength dflags - reservedStackSize) `div` spillSlotSize) - 1
        where is32Bit = target32Bit (targetPlatform dflags)
              reservedStackSize = (saveAreaBytes is32Bit) + (structRetPtrBytes is32Bit) + (requiredParamArrayBytes is32Bit)
