-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Rules
  ( allRules
  , rulesFor
  ) where

import Data.HashSet (HashSet)
import Prelude
import qualified Data.HashSet as HashSet

import Duckling.Dimensions
import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.Rules.Common as CommonRules
import qualified Duckling.Rules.ZH as ZHRules

-- | Returns the minimal set of rules required for `targets`.
rulesFor :: Locale -> HashSet (Some Dimension) -> [Rule]
rulesFor locale targets
  | HashSet.null targets = allRules locale
  | otherwise = [ rules | dims <- HashSet.toList $ explicitDimensions targets
                        , rules <- rulesFor' locale dims ]

-- | Returns all the rules for the provided locale.
-- We can't really use `allDimensions` as-is, since `TimeGrain` is not present.
allRules :: Locale -> [Rule]
allRules locale@(Locale lang _) = concatMap (rulesFor' locale) . HashSet.toList
  . explicitDimensions . HashSet.fromList $ allDimensions lang

rulesFor' :: Locale -> Some Dimension -> [Rule]
rulesFor' (Locale lang (Just region)) dim =
  CommonRules.rules dim ++ langRules lang dim ++ localeRules lang region dim
rulesFor' (Locale lang Nothing) dim =
  CommonRules.rules dim ++ defaultRules lang dim

-- | Default rules when no locale, for backward compatibility.
defaultRules :: Lang -> Some Dimension -> [Rule]
defaultRules ZH = ZHRules.defaultRules

localeRules :: Lang -> Region -> Some Dimension -> [Rule]
localeRules ZH = ZHRules.localeRules

langRules :: Lang -> Some Dimension -> [Rule]
langRules ZH = ZHRules.langRules
