-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1994-2004
--
-- -----------------------------------------------------------------------------

module SPARC.Regs (
        -- registers
        showReg,
        virtualRegSqueeze,
        realRegSqueeze,
        classOfRealReg,
        classOfReg,
        allRealRegs,
        fPair,

        -- machine specific info
        gReg, iReg, lReg, oReg, fReg,
        fp, sp, g0, g1, g2, g3, o0, o1, f0, f1, f6, f8, f22, f26, f27,

        -- allocatable
        allocatableRegs,

        -- args
        allArgRegs,
        callClobberedRegs,

        --
        mkVirtualReg,
        regDotColor
)

where


import GhcPrelude

import CodeGen.Platform.SPARC
import Reg
import RegClass
import Format

import Unique
import Outputable

{-
        The SPARC has 64 registers of interest; 32 integer registers and 32
        floating point registers.  The mapping of STG registers to SPARC
        machine registers is defined in StgRegs.h.  We are, of course,
        prepared for any eventuality.

        The whole fp-register pairing thing on sparcs is a huge nuisance.  See
        includes/stg/MachRegs.h for a description of what's going on
        here.
-}


-- | Get the standard name for the register with this number.
showReg :: RegNo -> String
showReg n
        | n >= 0  && n < 8   = "%g" ++ show n
        | n >= 8  && n < 16  = "%o" ++ show (n-8)
        | n >= 16 && n < 24  = "%l" ++ show (n-16)
        | n >= 24 && n < 32  = "%i" ++ show (n-24)
        | n >= 32 && n < 64  = "%f" ++ show (n-32)
        | otherwise          = panic "SPARC.Regs.showReg: unknown sparc register"


-- Get the register class of a certain real reg
classOfRealReg :: RealReg -> RegClass
classOfRealReg reg
 = case reg of
        RealRegSingle i
                | i < 32        -> RcInteger
                | otherwise     -> RcFloat

        RealRegPair{}           -> RcDouble

classOfReg :: Reg -> RegClass
classOfReg reg
 = case reg of
        RegReal rr    -> classOfRealReg rr
        RegVirtual vr -> classOfVirtualReg vr


-- | regSqueeze_class reg
--      Calculate the maximum number of register colors that could be
--      denied to a node of this class due to having this reg
--      as a neighbour.
--
{-# INLINE virtualRegSqueeze #-}
virtualRegSqueeze :: RegClass -> VirtualReg -> Int

virtualRegSqueeze cls vr
 = case cls of
        RcInteger
         -> case vr of
                VirtualRegI{}           -> 1
                VirtualRegHi{}          -> 1
                _other                  -> 0

        RcFloat
         -> case vr of
                VirtualRegF{}           -> 1
                VirtualRegD{}           -> 2
                _other                  -> 0

        RcDouble
         -> case vr of
                VirtualRegF{}           -> 1
                VirtualRegD{}           -> 1
                _other                  -> 0

        _other -> 0

{-# INLINE realRegSqueeze #-}
realRegSqueeze :: RegClass -> RealReg -> Int

realRegSqueeze cls rr
 = case cls of
        RcInteger
         -> case rr of
                RealRegSingle regNo
                        | regNo < 32    -> 1
                        | otherwise     -> 0

                RealRegPair{}           -> 0

        RcFloat
         -> case rr of
                RealRegSingle regNo
                        | regNo < 32    -> 0
                        | otherwise     -> 1

                RealRegPair{}           -> 2

        RcDouble
         -> case rr of
                RealRegSingle regNo
                        | regNo < 32    -> 0
                        | otherwise     -> 1

                RealRegPair{}           -> 1

        _other -> 0

-- | All the allocatable registers in the machine,
--      including register pairs.
allRealRegs :: [RealReg]
allRealRegs
        =  [ (RealRegSingle i)          | i <- [0..63] ]
        ++ [ (RealRegPair   i (i+1))    | i <- [32, 34 .. 62 ] ]


-- | Get the regno for this sort of reg
gReg, lReg, iReg, oReg, fReg :: Int -> RegNo

gReg x  = x             -- global regs
oReg x  = (8 + x)       -- output regs
lReg x  = (16 + x)      -- local regs
iReg x  = (24 + x)      -- input regs
fReg x  = (32 + x)      -- float regs


-- | Some specific regs used by the code generator.
g0, g1, g2, g3, fp, sp, o0, o1, f0, f1, f6, f8, f22, f26, f27 :: Reg

f6  = RegReal (RealRegSingle (fReg 6))
f8  = RegReal (RealRegSingle (fReg 8))
f22 = RegReal (RealRegSingle (fReg 22))
f26 = RegReal (RealRegSingle (fReg 26))
f27 = RegReal (RealRegSingle (fReg 27))

-- g0 is always zero, and writes to it vanish.
g0  = RegReal (RealRegSingle (gReg 0))
g1  = RegReal (RealRegSingle (gReg 1))
g2  = RegReal (RealRegSingle (gReg 2))
g3  = RegReal (RealRegSingle (gReg 3))

-- FP, SP, int and float return (from C) regs.
fp  = RegReal (RealRegSingle (iReg 6))
sp  = RegReal (RealRegSingle (oReg 6))
o0  = RegReal (RealRegSingle (oReg 0))
o1  = RegReal (RealRegSingle (oReg 1))
f0  = RegReal (RealRegSingle (fReg 0))
f1  = RegReal (RealRegSingle (fReg 1))

-- | Produce the second-half-of-a-double register given the first half or pair.
fPair :: Reg -> Reg
fPair (RegReal (RealRegSingle n))
 | n >= 32 && n `mod` 2 == 0 = RegReal (RealRegSingle (n+1))

fPair (RegReal (RealRegPair m n))
 | m >= 32 && m `mod` 2 == 0 && m + 1 == n = RegReal (RealRegSingle n)

fPair (RegVirtual (VirtualRegD u)) = RegVirtual (VirtualRegHi u)

fPair reg = panic ("MachInstrs.fPair: can't get high half of supposed double reg " ++ show reg)


-- | All the regs that the register allocator can allocate to,
--      with the the fixed use regs removed.
--
allocatableRegs :: [RealReg]
allocatableRegs
   = let isFree rr
           = case rr of
                RealRegSingle r     -> freeReg r
                RealRegPair   r1 r2 -> freeReg r1 && freeReg r2
     in filter isFree allRealRegs


-- | All the argument registers; each entry represents an argument slot.
--      Pairs of (Maybe intReg, Maybe doubleReg); the double reg is used for
--      64-bit only.
allArgRegs :: [(Maybe Reg, Maybe Reg)]
allArgRegs
        = maybeZip intRegs doubleRegs
        where
            intRegs = [RegReal $ RealRegSingle $ oReg i | i <- [0..5]]
            doubleRegs = [RegReal $ RealRegPair (fReg (2*i)) (fReg (2*i+1)) | i <- [0..16]]

            maybeZip :: [a] -> [b] -> [(Maybe a, Maybe b)]
            maybeZip [] []         = []
            maybeZip (a:as) []     = (Just a, Nothing) : maybeZip as []
            maybeZip []     (b:bs) = (Nothing, Just b) : maybeZip [] bs
            maybeZip (a:as) (b:bs) = (Just a, Just b)  : maybeZip as bs


-- These are the regs that we cannot assume stay alive over a C call.
-- %o6 is the C stack pointer; ABI mandates it is preserved.
--
callClobberedRegs :: [Reg]
callClobberedRegs
        = map (RegReal . RealRegSingle)
                (  oReg 7 :
                  [oReg i | i <- [0..5]] ++
                  [gReg i | i <- [1..7]] ++
                  [fReg i | i <- [0..31]] )



-- | Make a virtual reg with this format.
mkVirtualReg :: Unique -> Format -> VirtualReg
mkVirtualReg u format
        | not (isFloatFormat format)
        = VirtualRegI u

        | otherwise
        = case format of
                FF32    -> VirtualRegF u
                FF64    -> VirtualRegD u
                _       -> panic "mkVReg"


regDotColor :: RealReg -> SDoc
regDotColor reg
 = case classOfRealReg reg of
        RcInteger       -> text "blue"
        RcFloat         -> text "red"
        _other          -> text "green"
