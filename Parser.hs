module Parser where

import AST
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Char
import Text.Parsec.Language (emptyDef)
import Data.Char (isSpace)
import Data.Time
import Data.Time.Clock
import Data.List
import qualified Text.ParserCombinators.Parsec.Token as Token

-- definición de nustro lenguaje
languageDef :: TokenParser u
languageDef = makeTokenParser (emptyDef   { commentStart  = "/*"
                                          , commentEnd    = "*/"
                                          , commentLine   = "//"
                                          , reservedNames = ["crear", "abrir", "agregar", "ver", "modificar", "cancelar", "todos", "skip", "-desc", "-r", "-m", "-d", "-t", "-f", "-fd"]
                                          , reservedOpNames = ["/",":",";"]
                                          })

-- Donde comienza el parsing de todo el programa
parseComm :: SourceName -> String -> Either ParseError FileComm
parseComm = parse (totParser fileP)

-- Funciones para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do whiteSpace languageDef
                 t <- p
                 eof
                 return t

-- parser del tipo de dato FileComm. Un programa siempre debe empezar abriendo o creando un archivo
fileP :: Parser FileComm
fileP = newFileP
    <|> openFileP

newFileP :: Parser FileComm
newFileP = do reserved languageDef "crear"
              filename <- str
              reservedOp languageDef ";"
              seq <- sequenceOfComm
              return (New (filename ++ ".csv") seq)

openFileP :: Parser FileComm
openFileP = do reserved languageDef "abrir"
               filename <- str
               reservedOp languageDef ";"
               seq <- sequenceOfComm
               return (Open (filename ++ ".csv") seq)

-- dropWhileEnd elimina el sufijo más grande de una lista en la que se cumpla un predicado, que en nuestro caso es isSpace
str :: Parser String
str = do x   <- alphaNum   -- alphaNum :: Parser Char. Parsea un caracter del string. No falla si encuentra espacios
         sp  <- many space -- parsea espacios que pueda haber entre palabras
         xs  <- many str   -- se llama recursivamente para parsear cada caracter que compone el string
         let string = dropWhileEnd isSpace ([x]++(sp)++(concat xs)) -- string con el primer caracter encontrado + espacios y palabras. Le quito los espacios finales con dropWhileEnd
         return string

-- devuelve los comandos ya parseados en una lista. El separador entre cada comando es un punto y coma
sequenceOfComm = do list <- (sepBy1 comm' (semi languageDef))
                    return $ (listToSeq list)

comm' :: Parser Comm
comm' = try insertP
       <|>  selectP
       <|>  updateP
       <|>  cancelP
       <|>  skipComm

insertP = try insertFullDateP
      <|> try insertBetweenP
      <|> try insertAllDaysP
      <|> try insertWeeklyP
      <|> try insertMonthlyP 
      <|>     insertFullDay

selectP = try selectFullDateP
      <|> try selectBetweenP
      <|> try selectAllDaysP
      <|>     selectDayP
 
updateP = try updateDescriptionP
      <|> try updateFullDateP
      <|> try updateDateP
      <|>     updateTimeP

cancelP = try cancelDate
          <|> cancelDay

-- dada una lista de comandos parseados, genera el tipo de dato que representa una secuencia de comandos
listToSeq []     = Skip
listToSeq [x]    = x
listToSeq (x:xs) = Seq x (listToSeq xs)

skipComm :: Parser Comm
skipComm = reserved languageDef "skip" >> return Skip

insertFullDateP :: Parser Comm
insertFullDateP = do reserved languageDef "agregar"
                     date <- dateP
                     desc <- str
                     return (Insert date desc)

insertBetweenP :: Parser Comm
insertBetweenP = do reserved languageDef "agregar"
                    day1 <- dayP
                    reserved languageDef "-r"
                    day2 <- dayP
                    hour <- hourP
                    desc <- str
                    return (InsertBetween (UTCTime day1 hour) (UTCTime day2 hour) desc) -- devuelvo las dos fechas indicadas para iterar entre ellas

insertAllDaysP :: Parser Comm
insertAllDaysP = do reserved languageDef "agregar"
                    reserved languageDef "todos"
                    try (reservedOp languageDef "/")
                    month <-  natural languageDef
                    try (reservedOp languageDef "/")          
                    year <-  natural languageDef
                    hour <- hourP
                    desc <- str
                    return (InsertAllDays (UTCTime (makeDay year month 01) hour)
                                          (UTCTime (addGregorianMonthsClip 1 (makeDay year month 01)) hour)
                                          desc) -- devuelvo el mes indicado y el mes siguiente para poder iterar cada día dentro del primero

insertWeeklyP :: Parser Comm
insertWeeklyP = do reserved languageDef "agregar"
                   weekday <- str
                   try (reservedOp languageDef "/")
                   month <- natural languageDef
                   try (reservedOp languageDef "/")
                   year <- natural languageDef
                   hour <- hourP
                   desc <- str
                   return (InsertWeekly (UTCTime (makeDay year month 01) hour)
                                        (UTCTime (addGregorianMonthsClip 1 (makeDay year month 01)) hour)
                                        desc weekday) -- devuelvo el mes indicado y el mes siguiente para poder iterar cada semana dentro del primero

insertMonthlyP :: Parser Comm
insertMonthlyP = do reserved languageDef "agregar"
                    day <- natural languageDef
                    (try (reservedOp languageDef "/"))
                    reserved languageDef "todos"    
                    (try (reservedOp languageDef "/"))          
                    year <-  natural languageDef
                    hour <- hourP
                    desc <- str
                    return (InsertMonthly (UTCTime (makeDay year 01 day) hour)
                                          (UTCTime (makeDay year 12 day) hour)
                                          desc) -- devuelvo el primer y último mes del año para poder iterar todo el año

insertFullDay :: Parser Comm
insertFullDay = do reserved languageDef "agregar"
                   day1 <- dayP
                   reserved languageDef "-f"
                   desc <- str
                   return (InsertFullDay (UTCTime day1 (60*60*60 + 60*60*60)) desc) -- devuelvo la fecha con la hora en 23:59. la hora no importa porque insertamos según el día

selectDayP :: Parser Comm
selectDayP = do reserved languageDef "ver"
                day <- dayP
                return (SelectDate (UTCTime day (60*60*60 + 60*60*60)))

selectFullDateP :: Parser Comm
selectFullDateP = do reserved languageDef "ver"
                     date <- dateP
                     return (SelectFullDate date)

selectBetweenP :: Parser Comm
selectBetweenP = do reserved languageDef "ver"
                    day1 <- dayP
                    reserved languageDef "-r"
                    day2 <- dayP
                    return (SelectBetween (UTCTime day1 (12*60*60 + 60*60)) (UTCTime day2 (12*60*60 + 60*60)))

selectAllDaysP :: Parser Comm
selectAllDaysP = do reserved languageDef "ver"
                    reserved languageDef "todos"
                    try (reservedOp languageDef "/")
                    month <-  natural languageDef
                    try (reservedOp languageDef "/")          
                    year <-  natural languageDef
                    return (SelectAllDays (UTCTime (makeDay year month 01) (12*60*60 + 60*60))
                                          (UTCTime (addGregorianMonthsClip 1 (makeDay year month 01)) (12*60*60 + 60*60)))

updateDescriptionP :: Parser Comm
updateDescriptionP = do reserved languageDef "modificar"
                        date <- dateP
                        reserved languageDef "-desc"
                        desc <- str
                        return (UpdateDescription date desc)

updateFullDateP :: Parser Comm
updateFullDateP = do reserved languageDef "modificar"
                     date <- dateP
                     reserved languageDef "-fd"
                     newDate <- dateP
                     return (UpdateFullDate date newDate) 

updateDateP :: Parser Comm
updateDateP = do reserved languageDef "modificar"
                 date <- dateP
                 reserved languageDef "-d"
                 newDay <- dayP
                 return (UpdateDate date (UTCTime newDay (60*60*60 + 60*60*60))) 

updateTimeP :: Parser Comm
updateTimeP = do reserved languageDef "modificar"
                 date <- dateP
                 reserved languageDef "-t"
                 newTime <- hourP
                 return (UpdateTime date (UTCTime (makeDay 2020 01 01) newTime)) -- la fecha no importa porque modificamos la hora

cancelDate :: Parser Comm
cancelDate = do reserved languageDef "cancelar"
                reserved languageDef "-fd"
                date <- dateP
                return (CancelEventDate date)

cancelDay :: Parser Comm
cancelDay = do reserved languageDef "cancelar"
               reserved languageDef "-d"
               day <- dayP
               return (CancelEventDay (UTCTime day (60*60*60 + 60*60*60)))

dateP :: Parser UTCTime
dateP = do day  <- dayP
           hour <- hourP
           return (UTCTime day hour)

dayP :: Parser Day
dayP = do day    <- natural languageDef
          try (reservedOp languageDef "/")
          month  <-  natural languageDef
          try (reservedOp languageDef "/")          
          year   <-  natural languageDef
          return (makeDay year month day)

hourP :: Parser DiffTime
hourP = do hour <- natural languageDef
           reservedOp languageDef ":"
           minute <- natural languageDef
           return ((fromInteger hour*60*60) + (fromInteger minute*60))

-- construye un día dado año, mes y día. Para mejor legibilidad del parser
makeDay :: Integer -> Integer -> Integer -> Day
makeDay year month day = fromGregorian (fromInteger year) (fromInteger month) (fromInteger day)

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs) = if isSpace x then removeSpaces xs else (x:xs)