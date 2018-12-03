-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Weight.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Weight.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Milligram 2)
             [ "2毫克"
             , "2 mg"
             ]
  , examples (simple Liang 9)
             [ "9两"
             , "9 两"
             ]
  , examples (simple Gram 3)
             [ "3克"
             , "3g"
             , "3 g"
             ]
  , examples (simple Jin 8)
             [ "8 斤"
             , "8斤"
             ]
  , examples (simple Kilogram 4)
             [ "4 公斤"
             , "4 kg"
             , "4kg"
             ]
  , examples (simple Pound 1)
             [ "1 磅"
             , "1磅"
             , "1pound"
             , "1 pound"
             ]
  , examples (simple Tonne 5)
             [ "5 吨"
             , "5吨"
             , "5t"
             , "5 t"
             ]
  ]
