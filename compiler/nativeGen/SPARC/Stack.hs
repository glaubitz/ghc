module SPARC.Stack (
        stackBias,
        saveAreaBytes,
        spRel,
        fpRel,
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

stackBias _
 | otherwise = 2047

-- | Get the size of the register save area
saveAreaBytes :: Bool -> Int

saveAreaBytes is32Bit
 | is32Bit   = 64

saveAreaBytes _
 | otherwise = 128

-- | Get an AddrMode relative to the address in sp.
--      This gives us a stack relative addressing mode for volatile
--      temporaries and for excess call arguments.
--
spRel :: Bool
      -> Int            -- ^ stack offset in words, positive or negative
      -> AddrMode

spRel is32Bit n = AddrRegImm sp (ImmInt ((stackBias is32Bit) + n * (wordLength is32Bit)))


-- | Get an address relative to the frame pointer.
--      This doesn't work work for offsets greater than 13 bits; we just hope for the best
--
fpRel :: Bool -> Int -> AddrMode
fpRel is32Bit n
        = AddrRegImm fp (ImmInt ((stackBias is32Bit) + n * (wordLength is32Bit)))

-- | Convert a spill slot number to a *byte* offset, with no sign.
--
spillSlotToOffset :: DynFlags -> Int -> Int
spillSlotToOffset dflags slot
        | slot >= 0 && slot < maxSpillSlots dflags
        = (saveAreaBytes is32Bit) + spillSlotSize * slot

        | otherwise
        = pprPanic "spillSlotToOffset:"
                      (   text "invalid spill location: " <> int slot
                      $$  text "maxSpillSlots:          " <> int (maxSpillSlots dflags))
        where is32Bit = target32Bit (targetPlatform dflags)


-- | The maximum number of spill slots available on the C stack.
--      If we use up all of the slots, then we're screwed.
--
--      Why do we reserve 64 bytes, instead of using the whole thing??
--              -- BL 2009/02/15
--
maxSpillSlots :: DynFlags -> Int
maxSpillSlots dflags
        = ((spillAreaLength dflags - (saveAreaBytes is32Bit)) `div` spillSlotSize) - 1
        where is32Bit = target32Bit (targetPlatform dflags)
