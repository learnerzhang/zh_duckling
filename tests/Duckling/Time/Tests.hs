-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.Tests (tests) where

import Data.Aeson
import Data.Aeson.Types ((.:), parseMaybe, withObject)
import Data.String
import Data.Text (Text)
import Data.Time
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Testing.Asserts
import Duckling.Testing.Types
import Duckling.Time.Types
import Duckling.TimeGrain.Types

import Data.String
import Prelude
import Test.Tasty

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Testing.Asserts
import Duckling.Testing.Types
import Duckling.Time.Corpus
import qualified Duckling.Time.ZHCorpus as T
import qualified Duckling.Time.CN.Corpus as CN
import qualified Duckling.Time.HK.Corpus as HK
import qualified Duckling.Time.MO.Corpus as MO
import qualified Duckling.Time.TW.Corpus as TW

tests :: TestTree
tests = testGroup "Time Tests"
  [ timeFormatTest
  , timeIntersectTest
  , makeCorpusTest [This Time] T.corpus
  , localeTests
  ]

timeFormatTest :: TestTree
timeFormatTest = testCase "Format Test" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [This Time]) xs
  where
    xs = examplesCustom (parserCheck expected parseValue) ["现在"]
    expected = "2013-02-12T04:30:00.000-02:00"

parseValue :: Value -> Maybe Text
parseValue = parseMaybe . withObject "value object" $ \o -> o .: "value"

timeIntersectTest :: TestTree
timeIntersectTest = testCase "Intersect Test" $ mapM_ check
  [ ((t01, t01), Just t01)
  , ((t01, t12), Nothing)
  , ((t12, t13), Just t12)
  , ((t12, t34), Nothing)
  , ((t13, t23), Just t23)
  , ((t13, t24), Just t23)
  , ((t0_, t0_), Just t0_)
  , ((t0_, t1_), Nothing)
  , ((t0_, t01), Just t01)
  , ((t0_, t13), Nothing)
  , ((t0_, t2_), Nothing)
  , ((t0m, t13), Just t13)

  , ((t12, t01), Nothing)
  , ((t13, t12), Just t12)
  , ((t34, t12), Nothing)
  , ((t23, t13), Just t23)
  , ((t24, t13), Just t23)
  , ((t1_, t0_), Nothing)
  , ((t01, t0_), Just t01)
  , ((t13, t0_), Nothing)
  , ((t2_, t0_), Nothing)
  , ((t13, t0m), Just t13)

  , ((t13, t2m), Just t23)
  , ((t2m, t13), Just t23)
  ]
  where
    check :: ((TimeObject, TimeObject), Maybe TimeObject) -> IO ()
    check ((t1, t2), exp) =
      assertEqual "wrong intersect" exp $ timeIntersect t1 t2
    t0_ = TimeObject t0 Second Nothing
    t0m = TimeObject t0 Minute Nothing
    t01 = TimeObject t0 Second $ Just t1
    t1_ = TimeObject t1 Second Nothing
    t12 = TimeObject t1 Second $ Just t2
    t13 = TimeObject t1 Second $ Just t3
    t2_ = TimeObject t2 Second Nothing
    t2m = TimeObject t2 Minute Nothing
    t23 = TimeObject t2 Second $ Just t3
    t24 = TimeObject t2 Second $ Just t4
    t34 = TimeObject t3 Second $ Just t4
    t0 = UTCTime day 0
    t1 = UTCTime day 1
    t2 = UTCTime day 2
    t3 = UTCTime day 3
    t4 = UTCTime day 4
    day = fromGregorian 2017 2 8

localeTests :: TestTree
localeTests = testGroup "Locale Tests"
  [ testGroup "ZH_CN Tests"
    [ makeCorpusTest [This Time] $ withLocale T.corpus localeCN CN.allExamples
    ]
  , testGroup "ZH_HK Tests"
    [ makeCorpusTest [This Time] $ withLocale T.corpus localeHK HK.allExamples
    ]
  , testGroup "ZH_MO Tests"
    [ makeCorpusTest [This Time] $ withLocale T.corpus localeMO MO.allExamples
    ]
  , testGroup "ZH_TW Tests"
    [ makeCorpusTest [This Time] $ withLocale T.corpus localeTW TW.allExamples
    ]
  ]
  where
    localeCN = makeLocale ZH $ Just CN
    localeHK = makeLocale ZH $ Just HK
    localeMO = makeLocale ZH $ Just MO
    localeTW = makeLocale ZH $ Just TW