module SPARC.Cond (
        Cond(..),
        condUnsigned,
        condToSigned,
        condToUnsigned,
        condUnconditional
)

where

-- | Branch condition codes.
data Cond
        = ALWAYS
        | EQQ
        | GE
        | GEU
        | GTT
        | GU
        | LE
        | LEU
        | LTT
        | LU
        | NE
        | NEG
        | NEVER
        | POS
        | VC
        | VS
        deriving (Eq, Show)


condUnsigned :: Cond -> Bool
condUnsigned GU  = True
condUnsigned LU  = True
condUnsigned GEU = True
condUnsigned LEU = True
condUnsigned _   = False


condToSigned :: Cond -> Cond
condToSigned GU  = GTT
condToSigned LU  = LTT
condToSigned GEU = GE
condToSigned LEU = LE
condToSigned x   = x


condToUnsigned :: Cond -> Cond
condToUnsigned GTT = GU
condToUnsigned LTT = LU
condToUnsigned GE  = GEU
condToUnsigned LE  = LEU
condToUnsigned x   = x


condUnconditional :: Cond -> Bool
condUnconditional ALWAYS = True
condUnconditional NEVER  = True
condUnconditional _      = False
