
-- | Bits and pieces on the bottom of the module dependency tree.
--      Also import the required constants, so we know what we're using.
--
--      In the interests of cross-compilation, we want to free ourselves
--      from the autoconf generated modules like main/Constants

module SPARC.Base (
        wordLength,
        wordLengthInBits,
        wordFormat,
        spillAreaLength,
        spillSlotSize,
        extraStackArgsHere,
        fits13Bits,
        largeOffsetError
)

where

import DynFlags
import Panic

import Data.Int

is32BitPlatform :: NatM Bool
is32BitPlatform = do
    dflags <- getDynFlags
    return $ target32Bit (targetPlatform dflags)

wordLength :: Bool -> Int

wordLength is32Bit
 | is32Bit = 4

wordLength is32Bit
 | not is32Bit = 8

wordLengthInBits :: Bool -> Int
wordLengthInBits is32Bit
        = (wordLength is32Bit) * 8

wordFormat :: Bool -> Format

wordFormat is32Bit
 | is32Bit = II32

wordFormat is32Bit
 | not is32Bit = II64

wordWidth :: Bool -> Width

wordWidth is32Bit
 | is32Bit = W32

wordWidth is32Bit
 | not is32Bit = W64

-- Size of the available spill area
spillAreaLength :: DynFlags -> Int
spillAreaLength
        = rESERVED_C_STACK_BYTES

-- | We need 8 bytes because our largest registers are 64 bit.
spillSlotSize :: Int
spillSlotSize = 8


-- | We (allegedly) put the first six C-call arguments in registers;
--      where do we start putting the rest of them?
extraStackArgsHere :: Bool -> Int

extraStackArgsHere is32Bit
 | is32Bit = 23

extraStackArgsHere is32Bit
 | not is32Bit = 22


{-# SPECIALIZE fits13Bits :: Int -> Bool, Integer -> Bool #-}
-- | Check whether an offset is representable with 13 bits.
fits13Bits :: Integral a => a -> Bool
fits13Bits x = x >= -4096 && x < 4096

-- | Sadness.
largeOffsetError :: (Show a) => a -> b
largeOffsetError i
  = panic ("ERROR: SPARC native-code generator cannot handle large offset ("
                ++ show i ++ ");\nprobably because of large constant data structures;" ++
                "\nworkaround: use -fllvm on this module.\n")
