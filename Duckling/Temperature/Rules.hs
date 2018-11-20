-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Temperature.Rules
  ( rules
  ) where

import Data.String
import Prelude

import Duckling.Dimensions.Types
import Duckling.Temperature.Helpers
import Duckling.Temperature.Types (TemperatureData (..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Temperature.Types as TTemperature

ruleNumeralAsTemp :: Rule
ruleNumeralAsTemp = Rule
  { name = "number as temp"
  , pattern =
    [ dimension Numeral
    ]
  , prod = \case
      (Token Numeral nd:_) ->
        Just . Token Temperature $ valueOnly $ floor $ TNumeral.value nd
      _ -> Nothing
  }


ruleLatentTempDegrees :: Rule
ruleLatentTempDegrees = Rule
  { name = "<latent temp> degrees"
  , pattern =
    [ Predicate $ isValueOnly False
    , regex "度|°"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Degree td
      _ -> Nothing
  }

ruleTempCelcius :: Rule
ruleTempCelcius = Rule
  { name = "<temp> Celcius"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "(摄|攝)氏(°|度)|(°)C"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleCelciusTemp :: Rule
ruleCelciusTemp = Rule
  { name = "Celcius <temp>"
  , pattern =
    [ regex "(摄|攝)氏"
    , Predicate $ isValueOnly True
    , regex "度|°"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Celsius td
      _ -> Nothing
  }

ruleTempFahrenheit :: Rule
ruleTempFahrenheit = Rule
  { name = "<temp> Fahrenheit"
  , pattern =
    [ Predicate $ isValueOnly True
    , regex "(华|華)氏(°|度)|(°)F"
    ]
  , prod = \tokens -> case tokens of
      (Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

ruleFahrenheitTemp :: Rule
ruleFahrenheitTemp = Rule
  { name = "Fahrenheit <temp>"
  , pattern =
    [ regex "(华|華)氏"
    , Predicate $ isValueOnly True
    , regex "度|°"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Temperature td:_) -> Just . Token Temperature $
        withUnit TTemperature.Fahrenheit td
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumeralAsTemp
  ] ++ [ ruleCelciusTemp
  , ruleFahrenheitTemp
  , ruleLatentTempDegrees
  , ruleTempCelcius
  , ruleTempFahrenheit
  ]
