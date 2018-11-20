{-# LANGUAGE DeriveGeneric #-} -- LANGUAGE Option or LANGUAGE pragma
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.Identity.Types where

import Control.DeepSeq
import Data.Hashable
import Data.Text (Text)
import Data.Aeson 
import qualified Data.Text as Text
import GHC.Generics

import Prelude

import Duckling.Resolve (Resolve(..))

data IdentityData = IdentityData { value :: Text }
    deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve IdentityData where
    type ResolvedValue IdentityData = IdentityData
    resolve _ _ x = Just (x, False)

instance ToJSON IdentityData where
    toJSON IdentityData {value} = object [ "value" .= value ]
