
-- | Bits and pieces on the bottom of the module dependency tree.
--      Also import the required constants, so we know what we're using.
--
--      In the interests of cross-compilation, we want to free ourselves
--      from the autoconf generated modules like main/Constants

module SPARC.Base (
        is32BitPlatform,
        wordLength,
        wordLengthInBits,
        wordFormat,
        spillAreaLength,
        spillSlotSize,
        fits13Bits,
        largeOffsetError
)

where

import GhcPrelude

import NCGMonad
import Format
import Platform

import DynFlags
import Panic

import Data.Int()

is32BitPlatform :: NatM Bool
is32BitPlatform = do
    dflags <- getDynFlags
    return $ target32Bit (targetPlatform dflags)

wordLength :: Bool -> Int

wordLength is32Bit
 | is32Bit   = 4

wordLength _
 | otherwise = 8

wordLengthInBits :: Bool -> Int
wordLengthInBits is32Bit
        = (wordLength is32Bit) * 8

wordFormat :: Bool -> Format

wordFormat is32Bit
 | is32Bit   = II32

wordFormat _
 | otherwise = II64

-- Size of the available spill area
spillAreaLength :: DynFlags -> Int
spillAreaLength
        = rESERVED_C_STACK_BYTES

-- | We need 8 bytes because our largest registers are 64 bit.
spillSlotSize :: Int
spillSlotSize = 8


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
