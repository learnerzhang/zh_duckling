{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Duckling.Identity.Rules
( rules ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Regex.Types 
import Duckling.Types 

import Duckling.Identity.Helpers
import Duckling.Identity.Types
import qualified Duckling.Identity.Types as TIdentity


ruleIdentity :: Rule 
ruleIdentity = Rule 
    { name = "citizen identity"
    , pattern = [regex "((\\d{15})(\\d\\d[0-9xX])?)(?!\\d)"]
    , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        if isValidIdentityNumber match 
            then Just $ Token Identity IdentityData { TIdentity.value = match }  
            else Nothing
      _ -> Nothing
    }

rules :: [Rule]
rules = 
    [ ruleIdentity
    ]