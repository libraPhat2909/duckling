{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.RU.Rules
  ( rules
  ) where
  
import Prelude
import Data.Text (Text)
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (isGrain)
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Ordinal.Types (OrdinalData(..))
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData(..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG


ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ( "сейчас"          , TG.Second  ,  0, "сейчас"                 )
  , ( "сегодня"         , TG.Day     ,  0, "сегодня"              )
  , ( "tomorrow"        , TG.Day     ,  1, "завтра"                )
  , ( "yesterday"       , TG.Day     , -1, "вчера"                 )
  , ( "after tomorrow"  , TG.Day     ,  2, "послезавтра"           )
  , ( "before yesterday", TG.Day     , -2, "позавчера"             )
  , ( "EOM|End of month", TG.Month   ,  1, "(конец|конца) месяца" )
  , ( "EOY|End of year" , TG.Year    ,  1, "(конец|конца) года"   )
  ]  

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "понедельник","понедельни(к|ка|ку)|пн")
  , ( "вторник","вторни(к|ка|ку)|вт")
  , ( "среда","сред(а|у|е)|ср")
  , ( "четверг","четвер(г|га|гу)|чт")
  , ( "пятница","пятниц(а|ы|у)|пт")
  , ( "суббота","суббот(а|ы|у)|cб")
  , ( "воскресенье","воскресень(е|ю)|вс")
  ]

  

rules :: [Rule]
rules =
  [
  ]
  ++ruleDaysOfWeek
  ++ruleInstants
 
