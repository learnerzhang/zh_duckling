-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Weight.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Weight.Types
import Duckling.Weight.Corpus
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Asserts
import Duckling.Testing.Types


tests :: TestTree
tests = testGroup "ZH Weight Tests"
  [ makeCorpusTest [This Weight] corpus
  , ambiguousTests
  ]

ambiguousTests :: TestTree
ambiguousTests = testCase "Ambiguous Tests" $
  analyzedAmbiguousTest context testOptions
    (testText, [This Weight], predicates)
  where
    context = testContext {locale = makeLocale ZH Nothing}
    testText = "3千克"
    predicates = simpleCheck <$>
      [ simple Kilogram 3.0
      , simple Gram 3000.0
      ]