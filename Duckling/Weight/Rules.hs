-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Weight.Rules
  ( rules
  ) where


import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Weight.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Weight.Types as TWeight


ruleNumeralAsWeight :: Rule
ruleNumeralAsWeight = Rule
  { name = "number as weight"
  , pattern =
    [ dimension Numeral
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData {TNumeral.value = v}:_) ->
        Just . Token Weight $ weight v
      _ -> Nothing
  }


ruleWeightMilligrams :: Rule
ruleWeightMilligrams = Rule
  { name = "<Weight> Milligrams"
  , pattern =
    [ dimension Weight
    , regex "mg|毫克"
    ]
  , prod = \tokens -> case tokens of
      (Token Weight dd:_) ->
        Just . Token Weight $ withUnit TWeight.Milligram dd
      _ -> Nothing
  }

ruleWeightGrams :: Rule
ruleWeightGrams = Rule
  { name = "<Weight> Grams"
  , pattern =
    [ dimension Weight
    , regex "g|克"
    ]
  , prod = \tokens -> case tokens of
      (Token Weight dd:_) ->
        Just . Token Weight $ withUnit TWeight.Gram dd
      _ -> Nothing
  }

ruleWeightLiangs :: Rule
ruleWeightLiangs = Rule
  { name = "<Weight> Liangs"
  , pattern =
    [ dimension Weight
    , regex "两"
    ]
  , prod = \tokens -> case tokens of
      (Token Weight dd:_) ->
        Just . Token Weight $ withUnit TWeight.Liang dd
      _ -> Nothing
  }

ruleWeightJins :: Rule
ruleWeightJins = Rule
  { name = "<Weight> Jin"
  , pattern =
    [ dimension Weight
    , regex "斤"
    ]
  , prod = \tokens -> case tokens of
      (Token Weight dd:_) ->
        Just . Token Weight $ withUnit TWeight.Jin dd
      _ -> Nothing
  }


ruleWeightJinAndWeightLiang :: Rule
ruleWeightJinAndWeightLiang = Rule
  { name = "<Weight> Jin and <Weight> Liang "
  , pattern =
    [ dimension Weight
    , regex "斤"
    , dimension Weight
    , regex "两"
    ]
  , prod = \tokens -> case tokens of
      (Token Weight dd:_) ->
        Just . Token Weight $ withUnit TWeight.Jin dd
      _ -> Nothing
  }

ruleWeightKilogram :: Rule
ruleWeightKilogram = Rule
  { name = "<Weight> Kilogram"
  , pattern =
    [ dimension Weight
    , regex "kg|千克|公斤"
    ]
  , prod = \tokens -> case tokens of
      (Token Weight dd:_) ->
        Just . Token Weight $ withUnit TWeight.Kilogram dd
      _ -> Nothing
  }

ruleWeightPound :: Rule
ruleWeightPound = Rule
  { name = "<Weight> Pound"
  , pattern =
    [ dimension Weight
    , regex "pound|Pound|磅"
    ]
  , prod = \tokens -> case tokens of
      (Token Weight dd:_) ->
        Just . Token Weight $ withUnit TWeight.Pound dd
      _ -> Nothing
  }

ruleWeightTonne :: Rule
ruleWeightTonne = Rule
  { name = "<Weight> miles"
  , pattern =
    [ dimension Weight
    , regex "t|tonne|Tonne|吨"
    ]
  , prod = \tokens -> case tokens of
      (Token Weight dd:_) ->
        Just . Token Weight $ withUnit TWeight.Tonne dd
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralAsWeight
  ] 
  ++ [ ruleWeightMilligrams
  , ruleWeightLiangs
  , ruleWeightJins
  , ruleWeightJinAndWeightLiang
  , ruleWeightKilogram
  , ruleWeightGrams
  , ruleWeightPound
  , ruleWeightTonne
  ]
