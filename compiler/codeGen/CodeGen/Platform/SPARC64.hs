{-# LANGUAGE CPP #-}

module CodeGen.Platform.SPARC64 where

#define MACHREGS_NO_REGS 0
#define MACHREGS_sparc64 1
#include "../../../../includes/CodeGen.Platform.hs"

