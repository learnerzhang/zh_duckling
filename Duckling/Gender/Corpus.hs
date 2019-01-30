{-# LANGUAGE OverloadedStrings #-}

module Duckling.Gender.Corpus
(corpus) where

import Data.String

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Gender.Types

import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (GenderData "男")
             [ "男",
               "男性",
               "男人",
               "男生"
             ]
  , examples (GenderData "女")
             [ "女",
               "女性",
               "女人",
               "女生"
             ]
  ]
