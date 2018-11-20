{-# LANGUAGE OverloadedStrings #-}

module Duckling.Identity.Corpus
(corpus) where

import Prelude
import Data.String

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Identity.Types

import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat 
    [ examples (IdentityData "50010519761129945X")
               [ "50010519761129945X"
               ]
    , examples (IdentityData "350128198303274534")
               [ "350128198303274534"
               ]
    , examples (IdentityData "113343450321432")
               [ "113343450321432"
               ]
    , examples (IdentityData "352203198710052517")
               [ "352203198710052517"
               ]
    ]
