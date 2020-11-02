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
) where

import AST
import Data.Time

------ SELECT
-- ver "dia-mes-año"
getEventByDate :: String -> [String] -> [String]
getEventByDate date []          = []
getEventByDate date (x:xs)      =
        let dateTimeDescription = getDateTimeDescription x
            eventDate           = dateTimeDescription !! 0
            event               = if eventDate == date 
                                  then (x : getEventByDate date xs) 
                                  else getEventByDate date xs
        in event

-- ver "dia-mes-año" "hora:min"
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
-- cancelar "dia-mes-año"
cancelEventByDate :: String -> [String] -> [String]
cancelEventByDate _ []          = []
cancelEventByDate date (x:xs)   =
        let dateTimeDescription = getDateTimeDescription x
            eventDate           = dateTimeDescription !! 0
            evento              = if eventDate == date 
                                  then (cancelEventByDate date xs) 
                                  else (x : cancelEventByDate date xs)
        in evento

-- cancelar "dia-mes-año" "hora:min"
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
-- modificar "dia-mes-año" "hora:min" "descripcion nueva"
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

-- modificar "dia-mes-año" "hora:min" "dia-mes-año" "hora:min"
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

-- modificar "dia-mes-año" "dia-mes-año"
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

-- modificar "dia-mes-año" "hora:min" "hora:min"
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

getDateTimeDescription :: String -> [String]
getDateTimeDescription event = sepByComma event

printDate :: UTCTime -> String
printDate date = formatTime defaultTimeLocale "%d/%m/%Y" date

printTime :: UTCTime -> String
printTime date = formatTime defaultTimeLocale "%H:%M" date

addOneDay :: UTCTime -> UTCTime
addOneDay date = addUTCTime (realToFrac 86400) date -- agrego 86400 segundos, o sea un día

addOneWeek :: UTCTime -> UTCTime
addOneWeek date = addUTCTime (realToFrac 604800) date -- agrego 604800 segundos, o sea una semana

formatEvent :: String -> String -> String -> String
formatEvent date time desc = date ++ "," ++ time ++ "," ++ desc ++ "\n"

formatEventFullDay :: String -> String -> String
formatEventFullDay date desc = date ++ "," ++ "(todo el día)" ++ "," ++ desc ++ "\n"

getWeekDay :: String -> String
getWeekDay weekday = case weekday of "lunes"     -> "Monday"
                                     "martes"    -> "Tuesday"
                                     "miercoles" -> "Wednesday"
                                     "jueves"    -> "Thursday"
                                     "viernes"   -> "Friday"
                                     "sabado"    -> "Saturday"
                                     "domingo"   -> "Sunday"

-- devuelve la fecha del primer "weekday" del mes
-- por ejemplo: getDateByWeekDay 2020-10-01 "Wednesday" --> 2020/10/07
getDateByWeekDay :: UTCTime -> String -> UTCTime
getDateByWeekDay date weekday = if ((formatTime defaultTimeLocale "%A" date) == weekday)
                                then date
                                else getDateByWeekDay (addOneDay date) weekday