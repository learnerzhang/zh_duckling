-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-full-laziness #-}

module Main (main) where

import Control.Monad
import Data.Some
import System.Environment

import Duckling.Debug
import Duckling.Dimensions.Types
import Duckling.Locale

main :: IO ()
main = do
  (repeatCount :: Int) <- read . head <$> getArgs
  void $ replicateM repeatCount $ void $ do
    debug zh "My number is 123" [This PhoneNumber,This Distance,This Numeral,This Email]
    debug zh "Wednesday 5:00PM 3/29/2017" [This Numeral,This Time]
    debug zh "12:30pm" [This Time]
    debug zh "tomorrow at 4pm" [This Time]
    debug zh "Tomorrow at 12.30?" [This Time]
    debug zh "Wednesday 9am" [This Time]
    debug zh "Sure do! Will 11:30 work?" [This Time,This AmountOfMoney]
    debug zh "8:00am" [This Time]
    where
      zh = makeLocale ZH Nothing
