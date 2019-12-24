{-# LANGUAGE OverloadedStrings #-}
module Duckling.Time.RU.Corpus
  (corpus
  ,negativeCorpus
  )where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month, refTime)
import Duckling.TimeGrain.Types hiding (add)

context :: Context
context = testContext  {locale = makeLocale RU Nothing  }
corpus :: Corpus
corpus = (context, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "1 преложение"
      , "недавно"
      , "скоро"
      ]

allExamples :: [Example]
allExamples = concat
    [examples(datetime (2013, 2,12,0,0,0) Day)
           [ "сегодня"
           ]
    , examples(datetime(2013, 2,12,4,30,0) Second)
           [ "сейчас"
           ]   
    , examples (datetime(2013, 2,13,0,0,0) Day)
          [ "завтра"   
           ] 
{-    , examples (datetime(2013, 2,14,0,0,0) Day)
           [ "послезавтра"   
          ] 
    , examples (datetime(2013, 2,11,0,0,0) Day)
           [ "вчера"   
           ] 
    , examples (datetime(2013, 2,10,0,0,0) Day)
           [ "позавчера"   
           ] 
-}

     ]
