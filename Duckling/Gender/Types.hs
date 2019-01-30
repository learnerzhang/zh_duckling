{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Duckling.Gender.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude

import Duckling.Resolve (Resolve(..))

newtype GenderData = GenderData { value :: Text }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve GenderData where
  type ResolvedValue GenderData = GenderData
  resolve _ _ x = Just (x, False)

instance ToJSON GenderData where
  toJSON GenderData {value} = object [ "value" .= value ]
