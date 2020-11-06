module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Char
import Text.Parsec.Language (emptyDef)
import Data.Char (isSpace)
import Data.Time
import Data.Time.Clock
import qualified Text.ParserCombinators.Parsec.Token as Token
import AST
import System.IO.Unsafe

lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , reservedNames = ["agregar", "-r", "-m", "ver", "skip", "crear", "abrir", "modificar", "-desc", "-fd", "-d", "-t", "-f", "cancelar", "todos"]
                                  , reservedOpNames = ["/",":",";"]
                                  })

parseComm :: SourceName -> String -> Either ParseError FileComm
parseComm = parse (totParser fileP)

-- Funciones para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do whiteSpace lis
                 t <- p
                 eof
                 return t

fileP :: Parser FileComm
fileP = newFileP
    <|> openFileP

newFileP :: Parser FileComm
newFileP = do reserved lis "crear"
              filename <- str
              reservedOp lis ";"
              seq <- sequenceOfComm
              return (New (filename ++ ".csv") seq)

openFileP :: Parser FileComm
openFileP = do reserved lis "abrir"
               filename <- str
               reservedOp lis ";"
               seq <- sequenceOfComm
               return (Open (filename ++ ".csv") seq)

str :: Parser String
str = do sp  <- many space
         x   <- alphaNum
         sp2 <- many space
         xs  <- many str
         let string = (sp)++[x]++(sp2)++(concat xs)
             string2 = reverse $ removeSpaces $ reverse string
         return string2

sequenceOfComm = do list <- (sepBy1 comm' (semi lis))
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
          <|> selectDayP
 
updateP = try updateDescriptionP
      <|> try updateFullDateP
      <|> try updateDateP
      <|>     updateTimeP

cancelP = try cancelDate
          <|> cancelDay

listToSeq []     = Skip
listToSeq [x]    = x
listToSeq (x:xs) = Seq x (listToSeq xs)

skipComm :: Parser Comm
skipComm = reserved lis "skip" >> return Skip

insertFullDateP :: Parser Comm
insertFullDateP = do reserved lis "agregar"
                     date <- dateP
                     desc <- str
                     return (Insert date desc)

insertBetweenP :: Parser Comm
insertBetweenP = do reserved lis "agregar"
                    day1 <- dayP
                    reserved lis "-r"
                    day2 <- dayP
                    hour <- hourP
                    desc <- str
                    return (InsertBetween (UTCTime day1 hour) (UTCTime day2 hour) desc)

insertAllDaysP :: Parser Comm
insertAllDaysP = do reserved lis "agregar"
                    reserved lis "todos"
                    try (reservedOp lis "/")
                    month <-  natural lis
                    try (reservedOp lis "/")          
                    year <-  natural lis
                    hour <- hourP
                    desc <- str
                    return (InsertAllDays (UTCTime (fromGregorian (fromInteger year) (fromInteger month) (fromInteger 01))
                                          hour)
                                          (UTCTime (addGregorianMonthsClip 1 (fromGregorian (fromInteger year) (fromInteger month) (fromInteger 01)))
                                          hour) desc)

insertWeeklyP :: Parser Comm
insertWeeklyP = do reserved lis "agregar"
                   weekday <- str
                   try (reservedOp lis "/")
                   month <- natural lis
                   try (reservedOp lis "/")
                   year <- natural lis
                   hour <- hourP
                   desc <- str
                   return (InsertWeekly (UTCTime (fromGregorian (fromInteger year) (fromInteger month) (fromInteger 01)) hour)
                                        (UTCTime (addGregorianMonthsClip 1 (fromGregorian (fromInteger year) (fromInteger month) (fromInteger 01))) hour)
                                        desc weekday)

insertMonthlyP :: Parser Comm
insertMonthlyP = do reserved lis "agregar"
                    day <- natural lis
                    (try (reservedOp lis "/"))
                    reserved lis "todos"    
                    (try (reservedOp lis "/"))          
                    year <-  natural lis
                    hour <- hourP
                    desc <- str
                    return (InsertMonthly (UTCTime (fromGregorian (fromInteger year)  (fromInteger 1) (fromInteger day)) hour)
                                          (UTCTime (fromGregorian (fromInteger year) (fromInteger 12) (fromInteger day)) hour)
                                          desc)

insertFullDay :: Parser Comm
insertFullDay = do reserved lis "agregar"
                   day1 <- dayP
                   reserved lis "-f"
                   desc <- str
                   return (InsertFullDay (UTCTime day1 (60*60*60 + 60*60*60)) desc)

selectDayP :: Parser Comm
selectDayP = do reserved lis "ver"
                day <- dayP
                return (SelectDate (UTCTime day (60*60*60 + 60*60*60)))

selectFullDateP :: Parser Comm
selectFullDateP = do reserved lis "ver"
                     date <- dateP
                     return (SelectFullDate date)

updateDescriptionP :: Parser Comm
updateDescriptionP = do reserved lis "modificar"
                        date <- dateP
                        reserved lis "-desc"
                        desc <- str
                        return (UpdateDescription date desc)

updateFullDateP :: Parser Comm
updateFullDateP = do reserved lis "modificar"
                     date <- dateP
                     reserved lis "-fd"
                     newDate <- dateP
                     return (UpdateFullDate date newDate) 

updateDateP :: Parser Comm
updateDateP = do reserved lis "modificar"
                 date <- dateP
                 reserved lis "-d"
                 newDay <- dayP
                 return (UpdateDate date (UTCTime newDay (60*60*60 + 60*60*60))) 

updateTimeP :: Parser Comm
updateTimeP = do reserved lis "modificar"
                 date <- dateP
                 reserved lis "-t"
                 newTime <- hourP
                 return (UpdateTime date (UTCTime (fromGregorian (fromInteger 2020) (fromInteger 01) (fromInteger 01)) newTime)) 

cancelDate :: Parser Comm
cancelDate = do reserved lis "cancelar"
                reserved lis "-fd"
                date <- dateP
                return (CancelEventDate date)

cancelDay :: Parser Comm
cancelDay = do reserved lis "cancelar"
               reserved lis "-d"
               day <- dayP
               return (CancelEventDay (UTCTime day (60*60*60 + 60*60*60)))

dateP :: Parser UTCTime
dateP = do day  <- dayP
           hour <- hourP
           return (UTCTime day hour)

dayP :: Parser Day
dayP = do day    <- natural lis
          try (reservedOp lis "/")
          month  <-  natural lis
          try (reservedOp lis "/")          
          year   <-  natural lis
          return (fromGregorian (fromInteger year) (fromInteger month) (fromInteger day))

hourP :: Parser DiffTime
hourP = do hour <- natural lis
           reservedOp lis ":"
           minute <- natural lis
           return ((fromInteger hour*60*60) + (fromInteger minute*60))

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs) = if isSpace x then removeSpaces xs else (x:xs)
