-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Dimensions
  ( allDimensions
  , explicitDimensions
  ) where

import Data.HashSet (HashSet)
import Prelude
import qualified Data.HashSet as HashSet

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Dimensions.Common as CommonDimensions

allDimensions :: Lang -> [Some Dimension]
allDimensions lang = CommonDimensions.allDimensions

-- | Augments `targets` with all dependent dimensions.
explicitDimensions :: HashSet (Some Dimension) -> HashSet (Some Dimension)
explicitDimensions targets = HashSet.union targets deps
  where
    deps = HashSet.unions . map dependents $ HashSet.toList targets

-- | Ordinal depends on Numeral for JA, KO, and ZH.
dependents :: Some Dimension -> HashSet (Some Dimension)
dependents (This CreditCardNumber) = HashSet.empty
dependents (This Distance) = HashSet.singleton (This Numeral)
dependents (This Weight) = HashSet.singleton (This Numeral)
dependents (This Duration) = HashSet.fromList [This Numeral, This TimeGrain]
dependents (This Numeral) = HashSet.empty
dependents (This Email) = HashSet.empty
dependents (This AmountOfMoney) = HashSet.singleton (This Numeral)
dependents (This Ordinal) = HashSet.singleton (This Numeral)
dependents (This PhoneNumber) = HashSet.empty
dependents (This Quantity) = HashSet.singleton (This Numeral)
dependents (This RegexMatch) = HashSet.empty
dependents (This Temperature) = HashSet.singleton (This Numeral)
dependents (This Time) =
  HashSet.fromList [This Numeral, This Duration, This Ordinal, This TimeGrain]
dependents (This TimeGrain) = HashSet.empty
dependents (This Url) = HashSet.empty
dependents (This Volume) = HashSet.singleton (This Numeral)
dependents (This Identity) = HashSet.empty
dependents (This (CustomDimension dim)) = dimDependents dim
