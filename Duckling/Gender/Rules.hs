{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Gender.Rules
  ( rules
  ) where

import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Gender.Types (GenderData (..))
import qualified Duckling.Gender.Types as TGender
import Duckling.Types
import Duckling.Regex.Types

maleGender :: Rule
maleGender = Rule
  { name = "male"
  , pattern =
    [ regex "(\x7537)(\x6027|\x4eba|\x751f)?"
    ]
  , prod = \_ -> Just $ Token Gender GenderData {TGender.value = "男"}
  }

femaleGender :: Rule
femaleGender = Rule
  { name = "female"
  , pattern =
    [ regex "\x5973(\x6027|\x4eba|\x751f)?"
    ]
  , prod = \_ -> Just $ Token Gender GenderData {TGender.value = "女"}
  }

rules :: [Rule]
rules =
  [
  ] ++  [ maleGender
        , femaleGender]