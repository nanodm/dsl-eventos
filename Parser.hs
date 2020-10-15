module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Char
import Text.Parsec.Language (emptyDef)
import Data.Char (isSpace)
import Data.Dates
import Data.Time
import Data.Time.Clock
import qualified Text.ParserCombinators.Parsec.Token as Token
import AST

-- Funciones para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                whiteSpace lis
                t <- p
                eof
                return t


lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , reservedNames = ["agregar", "agregarEntre", "skip", "crear", "abrir"]
                                  , reservedOpNames = ["/","-",":"]
                                  })

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

comm = parens lis comm
     <|> sequenceOfComm

sequenceOfComm =
  do list <- (sepBy1 comm' (semi lis))
     -- If there's only one statement return it without using Seq.
--     return $ if (length list) == 1 then head list else Seq list
     return $ (listToSeq list)

comm' :: Parser Comm
comm' =    agregarP
       <|> agregarEntreFechasP
       <|> skipComm

listToSeq [] = Skip
listToSeq [x] = x
listToSeq (x:xs) = Seq x (listToSeq xs)

skipComm :: Parser Comm
skipComm = reserved lis "skip" >> return Skip

agregarP :: Parser Comm
agregarP = do  reserved lis "agregar"
               date <- dateP
               desc <- str
               return (Add date desc)

-- agregar 09/09/2020 -r 11/09/2020 12:50:00 "clases";
agregarEntreFechasP :: Parser Comm
agregarEntreFechasP = do reserved lis "agregarEntre"
                         day1 <- dayP
                         day2 <- dayP
                         hour <- hourP
                         desc <- str
                         return (AddBetween (UTCTime day1 hour) (UTCTime day2 hour) desc)

dateP :: Parser UTCTime
dateP = do
            day  <- dayP
            hour <- hourP
            return (UTCTime day hour)

dayP :: Parser Day
dayP = do
          day    <- natural lis
          (try (reservedOp lis "/") <|> (reservedOp lis "-"))
          month  <-  natural lis
          (try (reservedOp lis "/") <|> (reservedOp lis "-"))          
          year   <-  natural lis
          return (fromGregorian (fromInteger year) (fromInteger month) (fromInteger day))

hourP :: Parser DiffTime
hourP = do
           hour <- natural lis
           reservedOp lis ":"
           minute <- natural lis
           return ((fromInteger hour*60*60) + (fromInteger minute*60))

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (x:xs) = if isSpace x then removeSpaces xs else (x:xs)

str :: Parser String
str = do sp <- many space
         x <- alphaNum
         sp2 <- many space
         xs <- many str
         let string = (sp)++[x]++(sp2)++(concat xs)
             string2 = reverse $ removeSpaces $ reverse string
         return string2