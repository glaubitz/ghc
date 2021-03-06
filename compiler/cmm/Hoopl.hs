{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Hoopl (
    module Compiler.Hoopl,
    module Hoopl.Dataflow,
  ) where

import Compiler.Hoopl hiding
  ( (<*>), mkLabel, mkBranch, mkMiddle, mkLast, -- clashes with our MkGraph
    DataflowLattice, OldFact, NewFact, JoinFun,
    fact_bot, fact_join, joinOutFacts, mkFactBase,
    Unique,
    FwdTransfer(..), FwdRewrite(..), FwdPass(..),
    BwdTransfer(..), BwdRewrite(..), BwdPass(..),
    mkFactBase, Fact,
    mkBRewrite3, mkBTransfer3,
    mkFRewrite3, mkFTransfer3,

  )

import Hoopl.Dataflow
