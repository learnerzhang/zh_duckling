-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.Common
  ( rules
  , defaultRules
  , langRules
  , localeRules
  ) where

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Locale

import qualified Duckling.AmountOfMoney.Rules as AmountOfMoney
import qualified Duckling.CreditCardNumber.Rules as CreditCardNumber
import qualified Duckling.Distance.Rules as Distance
import qualified Duckling.Duration.Rules as Duration
import qualified Duckling.Email.Rules as Email
import qualified Duckling.Numeral.Rules as Numeral
import qualified Duckling.PhoneNumber.Rules as PhoneNumber
import qualified Duckling.Temperature.Rules as Temperature
import qualified Duckling.Ordinal.Rules as Ordinal
import qualified Duckling.Quantity.Rules as Quantity
import qualified Duckling.Temperature.Rules as Temperature
import qualified Duckling.Url.Rules as Url
import qualified Duckling.Volume.Rules as Volume
import qualified Duckling.Identity.Rules as Identity
import qualified Duckling.Time.Rules as Time
import qualified Duckling.Time.CN.Rules as TimeCN
import qualified Duckling.Time.HK.Rules as TimeHK
import qualified Duckling.Time.MO.Rules as TimeMO
import qualified Duckling.Time.TW.Rules as TimeTW
import qualified Duckling.TimeGrain.Rules as TimeGrain



rules :: Some Dimension -> [Rule]
rules (This AmountOfMoney) = AmountOfMoney.rules
rules (This CreditCardNumber) = CreditCardNumber.rules
rules (This Distance) = Distance.rules
rules (This Duration) = Duration.rules
rules (This Email) = Email.rules
rules (This Numeral) = Numeral.rules
rules (This Ordinal) = []
rules (This PhoneNumber) = PhoneNumber.rules
rules (This Quantity) = []
rules (This RegexMatch) = []
rules (This Temperature) = Temperature.rules
rules (This Time) = []
rules (This TimeGrain) = []
rules (This Url) = Url.rules
rules (This Volume) = Volume.rules
rules (This Identity) = Identity.rules
rules (This (CustomDimension dim)) = dimRules dim

defaultRules :: Some Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Some Dimension -> [Rule]
localeRules CN (This Time) = TimeCN.rules
localeRules HK (This Time) = TimeHK.rules
localeRules MO (This Time) = TimeMO.rules
localeRules TW (This Time) = TimeTW.rules
localeRules region (This (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Some Dimension -> [Rule]
langRules (This AmountOfMoney) = AmountOfMoney.rules
langRules (This CreditCardNumber) = []
langRules (This Distance) = Distance.rules
langRules (This Duration) = []
langRules (This Email) = []
langRules (This Numeral) = Numeral.rules
langRules (This Ordinal) = Ordinal.rules
langRules (This PhoneNumber) = []
langRules (This Quantity) = Quantity.rules
langRules (This RegexMatch) = []
langRules (This Temperature) = Temperature.rules
langRules (This Time) = Time.rules
langRules (This TimeGrain) = TimeGrain.rules
langRules (This Url) = []
langRules (This Volume) = []
langRules (This Identity) = Identity.rules
langRules (This (CustomDimension dim)) = dimLangRules ZH dim