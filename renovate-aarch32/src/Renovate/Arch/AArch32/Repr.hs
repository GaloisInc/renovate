{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module Renovate.Arch.AArch32.Repr (
  ARMKind,
  A32,
  T32,
  ARMRepr(..)
  ) where

import           Data.Parameterized.Classes
import qualified Renovate as R
import qualified SemMC.Architecture.AArch32 as SA

data ARMKind = A32 | T32
type A32 = 'A32
type T32 = 'T32

type instance R.InstructionArchReprKind SA.AArch32 = ARMKind

type instance R.InstructionArchRepr SA.AArch32 = ARMRepr

data ARMRepr tp where
  A32Repr :: ARMRepr A32
  T32Repr :: ARMRepr T32

instance TestEquality ARMRepr where
  testEquality A32Repr A32Repr = Just Refl
  testEquality T32Repr T32Repr = Just Refl
  testEquality _ _ = Nothing

instance OrdF ARMRepr where
  compareF A32Repr A32Repr = EQF
  compareF T32Repr T32Repr = EQF
  compareF A32Repr _ = GTF
  compareF T32Repr _ = LTF

