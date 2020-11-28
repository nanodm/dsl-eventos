module Functions
( getEventByDate,
  getEventByFullDate,
  cancelEventByDate,
  cancelEventByFullDate,
  updateEventDescription,
  updateEventFullDate,
  updateEventDate,
  updateEventTime,
  searchEventByFullDate,
  searchEventByDay,
  searchFullDayEvent,
  printDate,
  printTime,
  addOneDay,
  addOneWeek,
  formatEvent,
  formatEventFullDay,
  getWeekDay,
  getDateByWeekDay,
  pDate,
  utcTimeDayFix,
  addOneMonth
) where

import AST
import Data.Time

------ SELECT
-- devuelve uno o más eventos que coincidan con la fecha dada
getEventByDate :: String -> [String] -> [String]
getEventByDate date []          = []
getEventByDate date (x:xs)      =
        let dateTimeDescription = getDateTimeDescription x
            eventDate           = dateTimeDescription !! 0
            event               = if eventDate == date 
                                  then (x : getEventByDate date xs) 
                                  else getEventByDate date xs
        in event

-- devuelve uno o más eventos que coincidan con la fecha y hora dadas
getEventByFullDate :: String -> String -> [String] -> [String]
getEventByFullDate date time []     = []
getEventByFullDate date time (x:xs) =
        let dateTimeDescription     = getDateTimeDescription x
            eventDate               = dateTimeDescription !! 0
            eventTime               = dateTimeDescription !! 1
            event                   = if (eventDate == date && eventTime == time) 
                                      then x : getEventByFullDate date time xs 
                                      else getEventByFullDate date time xs
        in event

------ DELETE
-- borra uno o más eventos que coincidan con la fecha dada
cancelEventByDate :: String -> [String] -> [String]
cancelEventByDate _ []          = []
cancelEventByDate date (x:xs)   =
        let dateTimeDescription = getDateTimeDescription x
            eventDate           = dateTimeDescription !! 0
            evento              = if eventDate == date 
                                  then (cancelEventByDate date xs) 
                                  else (x : cancelEventByDate date xs)
        in evento

-- borra uno o más eventos que coincidan con la fecha y hora dadas
cancelEventByFullDate :: String -> String -> [String] -> [String]
cancelEventByFullDate date time []     = []
cancelEventByFullDate date time (x:xs) =
        let dateTimeDescription        = getDateTimeDescription x
            eventDate                  = dateTimeDescription !! 0
            eventTime                  = dateTimeDescription !! 1
            evento                     = if (eventDate == date && eventTime == time)
                                         then (cancelEventByFullDate date time xs)
                                         else (x : cancelEventByFullDate date time xs)
        in evento

------ UPDATE
-- modifica la descripción de un evento que coincida con la fecha y hora dadas
updateEventDescription :: String -> String -> String -> [String] -> [String]
updateEventDescription _ _ _ []                 = []
updateEventDescription date time newDesc (x:xs) =
        let dateTimeDescription                 = getDateTimeDescription x
            eventDate                           = dateTimeDescription !! 0
            eventTime                           = dateTimeDescription !! 1
            newList                             = if (eventDate == date && eventTime == time)
                                                  then (date ++ "," ++ eventTime ++ "," ++ newDesc):xs
                                                  else x:(updateEventDescription date time newDesc xs)
        in newList

-- modifica la fecha y hora de un evento que coincida con la fecha y hora dadas
updateEventFullDate :: String -> String -> String -> String -> [String] -> [String]
updateEventFullDate _ _ _ _ []                        = []
updateEventFullDate date time newDate newTime (x:xs)  =
        let dateTimeDescription                       = getDateTimeDescription x
            eventDate                                 = dateTimeDescription !! 0
            eventTime                                 = dateTimeDescription !! 1
            eventDesc                                 = dateTimeDescription !! 2
            newList                                   = if (eventDate == date && eventTime == time)
                                                        then (newDate ++ "," ++ newTime ++ "," ++ eventDesc):xs
                                                        else x:(updateEventFullDate date time newDate newTime xs)
        in newList

-- modifica la fecha de un evento que coincida con la fecha dada
updateEventDate :: String -> String -> [String] -> [String]
updateEventDate _ _ []               = []
updateEventDate date newDate (x:xs)  =
        let dateTimeDescription      = getDateTimeDescription x
            eventDate                = dateTimeDescription !! 0
            eventTime                = dateTimeDescription !! 1
            eventDesc                = dateTimeDescription !! 2
            newList                  = if (eventDate == date)
                                       then (newDate ++ "," ++ eventTime ++ "," ++ eventDesc):xs
                                       else x:(updateEventDate date newDate xs)
        in newList

-- modifica la hora de un evento que coincida con la fecha y hora dadas
updateEventTime :: String -> String -> String -> [String] -> [String]
updateEventTime _ _ _ []                  = []
updateEventTime date time newTime (x:xs)  =
        let dateTimeDescription           = getDateTimeDescription x
            eventDate                     = dateTimeDescription !! 0
            eventTime                     = dateTimeDescription !! 1
            eventDesc                     = dateTimeDescription !! 2
            newList                       = if (eventDate == date && eventTime == time)
                                            then (date ++ "," ++ newTime ++ "," ++ eventDesc):xs
                                            else x:(updateEventTime date time newTime xs)
        in newList

-- devuelve un evento que coincida con la fecha y hora dadas
searchEventByFullDate :: String -> String -> [String] -> Bool
searchEventByFullDate _ _ []           = False
searchEventByFullDate date time (x:xs) =
        let dateTimeDescription        = getDateTimeDescription x
            eventDate                  = dateTimeDescription !! 0
            eventTime                  = dateTimeDescription !! 1
            hasEvent                   = if (eventDate == date && eventTime == time)
                                         then True
                                         else (searchEventByFullDate date time xs)
        in hasEvent

-- devuelve un evento que coincida con la fecha dada
searchFullDayEvent :: String -> [String] -> Bool
searchFullDayEvent _ []         = False
searchFullDayEvent date (x:xs)  =
        let dateTimeDescription = getDateTimeDescription x
            eventDate           = dateTimeDescription !! 0
            eventTime           = dateTimeDescription !! 1
            hasEvent            = if (eventDate == date && eventTime == "(todo el día)")
                                  then True
                                  else (searchFullDayEvent date xs)
        in hasEvent

-- devuelve un evento (que dure todo el día) que coincida con la fecha dada
searchEventByDay :: String -> [String] -> Bool
searchEventByDay _ []           = False
searchEventByDay date (x:xs)    =
        let dateTimeDescription = getDateTimeDescription x
            eventDate           = dateTimeDescription !! 0
            hasEvent            = if (eventDate == date)
                                  then True
                                  else (searchEventByDay date xs)
        in hasEvent


---------------------- FUNCIONES AUXILIARES

--- sepByComma agarra una linea separada por comas y devuelve una lista con las palabras de la misma
--- > sepByComma "09-09-2020,21:30,Cocinar"
--- ["09-09-2020","21:30","Cocinar"]
-- Sintaxis robada de la función words de Data.List -> https://www.haskell.org/onlinereport/standard-prelude.html
-- dropWhile: crea una lista a partir de otra, inspecciona la lista original y toma de ella sus elementos desde el momento en que la condición falla por primera vez hasta el final de la lista
sepByComma     :: String -> [String]
sepByComma s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : sepByComma s''
                            where (w, s'') = break (==',') s'

-- dado un evento del tipo "fecha,dia,hora" devuelve una lista ["fecha","dia","hora"]
getDateTimeDescription :: String -> [String]
getDateTimeDescription event = sepByComma event

-- devuelve un String con la fecha en el formato dia/mes/año
printDate :: UTCTime -> String
printDate date = formatTime defaultTimeLocale "%d/%m/%Y" date

-- devuelve un String con la hora en el formato hh:mm
printTime :: UTCTime -> String
printTime date = formatTime defaultTimeLocale "%H:%M" date

-- le agrego 86400 segundos (un día) a un UTCTime
addOneDay :: UTCTime -> UTCTime
addOneDay date = addUTCTime (realToFrac 86400) date

-- le agrego 604800 segundos (una semana) a un UTCTime
addOneWeek :: UTCTime -> UTCTime
addOneWeek date = addUTCTime (realToFrac 604800) date

addOneMonth :: UTCTime -> UTCTime
addOneMonth date = UTCTime (addGregorianMonthsClip 1 (pDate date)) (getTime date)

-- dados 3 strings con fecha, hora y descripción devuelvo un String del tipo "fecha,hora,descripción"
formatEvent :: String -> String -> String -> String
formatEvent date time desc = date ++ "," ++ time ++ "," ++ desc ++ "\n"

formatEventFullDay :: String -> String -> String
formatEventFullDay date desc = date ++ "," ++ "(todo el día)" ++ "," ++ desc ++ "\n"

-- dado un día en español, se devuelve el String que corresponde al día en inglés para el tipo de dato UTCTime
getWeekDay :: String -> String
getWeekDay weekday = case weekday of "lunes"     -> "Monday"
                                     "martes"    -> "Tuesday"
                                     "miercoles" -> "Wednesday"
                                     "jueves"    -> "Thursday"
                                     "viernes"   -> "Friday"
                                     "sabado"    -> "Saturday"
                                     "domingo"   -> "Sunday"

-- devuelve la fecha del primer "weekday" del mes. Se llama recursivamente agregando de a un día hasta encontrar la fecha correspondiente
-- por ejemplo: getDateByWeekDay 2020-10-01 "Wednesday" --> 2020/10/07
getDateByWeekDay :: UTCTime -> String -> UTCTime
getDateByWeekDay date weekday = if ((formatTime defaultTimeLocale "%A" date) == weekday) -- "%A" devuelve como String el día de la semana dada una fecha
                                then date
                                else getDateByWeekDay (addOneDay date) weekday

getMonth :: UTCTime -> Integer
getMonth date = read (formatTime defaultTimeLocale "%m" date) :: Integer

getTime :: UTCTime -> DiffTime
getTime date = (fromInteger (read(formatTime defaultTimeLocale "%H" date) :: Integer) *60*60) + (fromInteger (read(formatTime defaultTimeLocale "%M" date) :: Integer)*60)

pDate :: UTCTime -> Day
pDate date = fromGregorian  (read (formatTime defaultTimeLocale "%Y" date) :: Integer)  (fromInteger (getMonth date)) (fromInteger(read (formatTime defaultTimeLocale "%d" date) :: Integer))

-- para formar la fecha toma el número de día de dateR, el resto de los datos son tomados de dateWr
utcTimeDayFix :: UTCTime -> UTCTime -> UTCTime
utcTimeDayFix dateWr dateR = UTCTime (fromGregorian (read (formatTime defaultTimeLocale "%Y" dateWr) :: Integer)  (fromInteger (getMonth dateWr)) (fromInteger(read (formatTime defaultTimeLocale "%d" dateR) :: Integer))) (getTime dateWr)
