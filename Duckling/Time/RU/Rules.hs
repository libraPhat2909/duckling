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

--дата 
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

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "Январь", "Янв(арь|аря|аре)|янв\\.?"   )
  , ( "февраль", "февр(аль|аля|але)|фев\\.?"   )
  , ( "март", "март(а|е)?|ма\\.?" )
  , ( "апрель", "апр(ель|еля|еле)|апр\\.?"  )
  , ( "май", "(май|мая|мае)|май\\.?"  )
  , ( "июнь", "и(юнь|юня|юне)|июн\\.?"  )
  , ( "июль", "и(юль|юля|юле)|июл\\.?"   )
  , ( "август", "август(а|е)?|авг\\.?"  )
  , ( "сентябрь", "сент(ябрь|ября|ябре)|сент\\.?" )
  , ( "октябрь", "окт(ябрь|ября|ябре)|окт\\.?"  )
  , ( "ноябрь", "ноя(брь|бря|бре)?|ноя\\.?"    )
  , ( "декабрь", "дек(абрь|абря|абре)|дек\\.?"  )
  ]

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isNotLatent
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleDayofmonthNonOrdinalNamedmonth :: Rule
ruleDayofmonthNonOrdinalNamedmonth = Rule
  { name = "<day-of-month> (non ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMInteger
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDayofmonthordinalNamedmonth :: Rule
ruleDayofmonthordinalNamedmonth = Rule
  { name = "<day-of-month>(ordinal) <named-month>"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleMmdd :: Rule
ruleMmdd = Rule
  { name = "mm/dd"
  , pattern =
    [ regex "([012]?[1-9]|10|20|30|31)\\.(10|11|12|0?[1-9])\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        d <- parseInt m1
        m <- parseInt m2
        tt $ monthDay m d
      _ -> Nothing
  }

ruleYyyymmdd :: Rule
ruleYyyymmdd = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})-(0?[1-9]|10|11|12)-([012]?[1-9]|10|20|30|31)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:m3:_)):_) -> do
        y <- parseInt m1
        m <- parseInt m2
        d <- parseInt m3
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

ruleDayofmonthordinalNamedmonthYear :: Rule
ruleDayofmonthordinalNamedmonthYear = Rule
  { name = "<day-of-month>(ordinal) <named-month> year"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    , regex "(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (token:
       Token Time td:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> do
        n <- parseInt match
        dom <- intersectDOM td token
        Token Time <$> intersect dom (year n)
      _ -> Nothing
  }

ruleYearLatent2 :: Rule
ruleYearLatent2 = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 2101 10000
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        v <- getIntValue token
        tt . mkLatent $ year v
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "year (latent)"
  , pattern =
    [ Predicate $
        or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 999]
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        y <- getIntValue token
        tt . mkLatent $ year y
      _ -> Nothing
  }


ruleYear :: Rule
ruleYear = Rule
  { name = "year"
  , pattern =
    [ Predicate $ isIntegerBetween 1000 2100
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        y <- getIntValue token
        tt $ year y
      _ -> Nothing
  }

ruleOnDate :: Rule
ruleOnDate = Rule
  { name = "on <date>"
  , pattern =
    [ regex "в"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:x:_) -> Just x
      _ -> Nothing
  }

ruleInDuration :: Rule
ruleInDuration = Rule
  { name = "in <duration>"
  , pattern =
    [ regex "через"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) ->
        tt $ inDuration dd
      _ -> Nothing
  }

ruleDurationAgo :: Rule
ruleDurationAgo = Rule
  { name = "<duration> ago"
  , pattern =
    [ dimension Duration
    , regex "назад"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_:_) ->
        tt $ durationAgo dd
      _ -> Nothing
  }

--время
ruleAtTimeofday :: Rule
ruleAtTimeofday = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "в"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }
ruleTimeofdayOclock :: Rule
ruleTimeofdayOclock = Rule
  { name = "<time-of-day>  o'clock"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(час|часа|часов)?|ч(?:[\\s'\"-_{}\\[\\]()]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) ->
        tt $ notLatent td
      _ -> Nothing
  }
ruleTimePartofday :: Rule
ruleTimePartofday = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

ruleTimeofdayLatent :: Rule
ruleTimeofdayLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 0 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour (n < 12) n
      _ -> Nothing
  }

ruleMorning :: Rule
ruleMorning = Rule
  { name = "morning"
  , pattern =
    [ regex "утра|утром"
    ]
  , prod = \_ ->
      let from = hour False 3
          to = hour False 12
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleAfternoon :: Rule
ruleAfternoon = Rule
  { name = "afternoon"
  , pattern =
    [ regex "дня|днем"
    ]
  , prod = \_ ->
      let from = hour False 12
          to = hour False 19
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleEvening :: Rule
ruleEvening = Rule
  { name = "evening"
  , pattern =
    [ regex "вечера|вечером"
    ]
  , prod = \_ ->
      let from = hour False 18
          to = hour False 0
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }

ruleNight :: Rule
ruleNight = Rule
  { name = "night"
  , pattern =
    [ regex "ночи"
    ]
  , prod = \_ ->
      let from = hour False 0
          to = hour False 4
      in Token Time . mkLatent . partOfDay <$>
           interval TTime.Open from to
  }
ruleHhmm :: Rule
ruleHhmm = Rule
  { name = "hh:mm"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))[:.ч]([0-5]\\d)(?:(час|часа|часов)?|ч)?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:m2:_)):_) -> do
        h <- parseInt m1
        m <- parseInt m2
        tt $ hourMinute False h m
      _ -> Nothing
  }

ruleHhmmMilitary :: Rule
ruleHhmmMilitary = Rule
  { name = "hhmm (military)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (h:m:_)):_) -> do
        hh <- parseInt h
        mm <- parseInt m
        tt . mkLatent $ hourMinute False hh mm
      _ -> Nothing
  }
rules :: [Rule]
rules =
  [ruleIntersect 
  , ruleDayofmonthNonOrdinalNamedmonth
  , ruleDayofmonthordinalNamedmonth
  , ruleMmdd
  , ruleYyyymmdd
  , ruleDayofmonthordinalNamedmonthYear
  , ruleYearLatent2
  , ruleYearLatent
  , ruleYear
  , ruleOnDate
  , ruleInDuration
  , ruleDurationAgo
  , ruleAtTimeofday
  , ruleTimePartofday 
  , ruleMorning
  , ruleAfternoon
  , ruleEvening
  , ruleNight
  , ruleHhmm
  , ruleHhmmMilitary
  , ruleTimeofdayOclock
  , ruleTimeofdayLatent
  ]
  ++ruleDaysOfWeek
  ++ruleInstants
  ++ruleMonths
 
