-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}

module Duckling.Weight.Helpers
  ( weight
  , isWeightOfUnit
  , isSimpleWeight
  , unitOnly
  , withInterval
  , withMax
  , withMin
  , withUnit
  , withValue
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Weight.Types (WeightData(..))
import Duckling.Types
import qualified Duckling.Weight.Types as TWeight

-- -----------------------------------------------------------------
-- Patterns

isSimpleWeight :: Predicate
isSimpleWeight (Token Weight WeightData {TWeight.value = Just _
                                              , TWeight.unit = Just _}) = True
isSimpleWeight _ = False

isWeightOfUnit :: TWeight.Unit -> Predicate
isWeightOfUnit unit (Token Weight WeightData {TWeight.unit = Just u}) = unit == u
isWeightOfUnit _ _ = False

-- -----------------------------------------------------------------
-- Production

weight :: Double -> WeightData
weight x = WeightData {TWeight.value = Just x
                          , TWeight.unit = Nothing
                          , TWeight.minValue = Nothing
                          , TWeight.maxValue = Nothing}

unitOnly :: TWeight.Unit -> WeightData
unitOnly u = WeightData {TWeight.unit = Just u
                          , TWeight.value = Nothing
                          , TWeight.minValue = Nothing
                          , TWeight.maxValue = Nothing}

withUnit :: TWeight.Unit -> WeightData -> WeightData
withUnit u dd = dd {TWeight.unit = Just u}

withValue :: Double -> WeightData -> WeightData
withValue value dd = dd {TWeight.value = Just value}

withInterval :: (Double, Double) -> WeightData -> WeightData
withInterval (from, to) dd = dd {TWeight.minValue = Just from
                                , TWeight.maxValue = Just to}

withMin :: Double -> WeightData -> WeightData
withMin from dd = dd {TWeight.minValue = Just from}

withMax :: Double -> WeightData -> WeightData
withMax to dd = dd {TWeight.maxValue = Just to}
