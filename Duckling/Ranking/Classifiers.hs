-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


module Duckling.Ranking.Classifiers
  ( classifiers
  ) where

import Data.Maybe

import Duckling.Locale
import Duckling.Ranking.Types
import qualified Duckling.Ranking.Classifiers.ZH_XX as ZH_XXClassifiers

classifiers :: Locale -> Classifiers
classifiers (Locale ZH _) = ZH_XXClassifiers.classifiers
