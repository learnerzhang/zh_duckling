-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Api.Tests
  ( tests
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import Prelude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import Duckling.Api
import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Testing.Asserts
import Duckling.Testing.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

tests :: TestTree
tests = testGroup "API Tests"
  [ parseTest
  , rankTest
  , rangeTest
  , supportedDimensionsTest
  ]

parseTest :: TestTree
parseTest = testCase "Parse Test" $
  case parse sentence testContext testOptions [This Numeral] of
    [] -> assertFailure "empty result"
    (Entity dim body (RVal _ v) start end _ _:_) -> do
      assertEqual "dim" "number" dim
      assertEqual "body" "42" body
      assertEqual "value" val (toJText v)
      assertEqual "start" 4 start
      assertEqual "end" 6 end
  where
    sentence = "hey 42 there"
    val = toJText TNumeral.NumeralValue {TNumeral.vValue = 42.0}

rankTest :: TestTree
rankTest = testGroup "Rank Tests"
  [ rankFilterTest
  , rankOrderTest
  ]

rankFilterTest :: TestTree
rankFilterTest = testCase "Rank Filter Tests" $ do
  mapM_ check
    [ ( "2分钟"
      , [This Numeral, This Duration, This Time]
      , [This Duration]
      )
    , ( "2分钟, 大概 42 度"
      , [This Numeral, This Temperature, This Duration, This Time]
      , [This Duration, This Temperature]
      )
    , ( "今天工作...和明天9pm"
      , [This Numeral, This Time]
      , [This Time, This Time]
      )
    , ( "星期二或者星期三或者下周"
      , [This Numeral, This Time]
      , [This Time, This Time, This Time]
      )
    , ("后天 5pm", [This Time], [This Time])
    , ("后天 5pm", [This Time, This Numeral], [This Time])
    , ("后天 5pm", [], [This Time])
    ]
  where
    check :: (Text, [Some Dimension], [Some Dimension]) -> IO ()
    check (sentence, targets, expected) =
      let go = analyze sentence testContext testOptions $ HashSet.fromList targets
          actual = flip map go $
                     \(Resolved{node=Node{token=Token d _}}) -> This d
      in assertEqual ("wrong winners for " ++ show sentence) expected actual

rankOrderTest :: TestTree
rankOrderTest = testCase "Rank Order Tests" $ do
  mapM_ check
    [ ("tomorrow at 5PM or 8PM", [This Time])
    , ("321 12 3456 ... 7", [This Numeral])
    , ("42 today 23 tomorrow", [This Numeral, This Time])
    ]
  where
    check (s, targets) =
      let tokens = analyze s testContext testOptions $ HashSet.fromList targets
        in assertEqual "wrong ordering" (sortOn range tokens) tokens

rangeTest :: TestTree
rangeTest = testCase "Range Tests" $ do
  mapM_ (analyzedFirstTest testContext testOptions) xs
  where
    xs = map (\(input, targets, range) -> (input, targets, f range))
             [ ( "order status 3233763377", [This PhoneNumber], Range 13 23 )
             , ( "  3233763377  "         , [This PhoneNumber], Range  2 12 )
             , ( " -3233763377"           , [This PhoneNumber], Range  2 12 )
             , ( "  现在"                  , [This Time]       , Range  2  4 )
             , ( "   星期一  "            , [This Time]       , Range  3  6 )
             , ( "  下周 "         , [This Time]       , Range  2 4 )
             , ( "   42\n\n"              , [This Numeral]    , Range  3  5 )
             ]
    f :: Range -> TestPredicate
    f expected _ (Resolved {range = actual}) = expected == actual

supportedDimensionsTest :: TestTree
supportedDimensionsTest = testCase "Supported Dimensions Test" $ do
  mapM_ check
    [ ( ZH
      , [ This Url,This Quantity,This Temperature
        ,This Time,This AmountOfMoney,This Numeral
        ,This Ordinal,This PhoneNumber,This Identity
        ,This Distance,This Weight,This Duration
        ,This Email
        ]
      )
    ]
  where
    check :: (Lang, [Some Dimension]) -> IO ()
    check (l, expected) = case HashMap.lookup l supportedDimensions of
      Nothing -> assertFailure $ "no dimensions for " ++ show l
      Just actual ->
        assertEqual ("wrong dimensions for " ++ show l)
        (HashSet.fromList expected) (HashSet.fromList actual)
