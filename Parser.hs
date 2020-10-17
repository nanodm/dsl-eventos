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
                                  , reservedNames = ["agregar", "agregarEntre", "ver", "skip", "crear", "abrir"]
                                  , reservedOpNames = ["/","-",":",";"]
                                  })

-- parseComm :: SourceName -> String -> Either ParseError Comm
-- parseComm = parse (totParser comm)

parseComm = parse (totParser fileP)

fileP :: Parser FileComm
fileP =   newFileP
      <|> openFileP

comm = parens lis comm
     <|> sequenceOfComm

sequenceOfComm =
  do list <- (sepBy1 comm' (semi lis))
     -- If there's only one statement return it without using Seq.
--     return $ if (length list) == 1 then head list else Seq list
     return $ (listToSeq list)

comm' :: Parser Comm
comm' =    insertP
       <|> insertBetweenP
       <|> selectP
       <|> skipComm

listToSeq [] = Skip
listToSeq [x] = x
listToSeq (x:xs) = Seq x (listToSeq xs)

newFileP :: Parser FileComm
newFileP = do reserved lis "crear"
              filename <- str
              reservedOp lis ";"
              seq <- comm
              return (New (filename ++ ".csv") seq)

openFileP :: Parser FileComm
openFileP = do reserved lis "abrir"
               filename <- str
               reservedOp lis ";"
               seq <- comm
               return (Open (filename ++ ".csv") seq)

skipComm :: Parser Comm
skipComm = reserved lis "skip" >> return Skip

insertP :: Parser Comm
insertP = do  reserved lis "agregar"
              date <- dateP
              desc <- str
              return (Insert date desc)

insertBetweenP :: Parser Comm
insertBetweenP = do reserved lis "agregarEntre"
                    day1 <- dayP
                    day2 <- dayP
                    hour <- hourP
                    desc <- str
                    return (InsertBetween (UTCTime day1 hour) (UTCTime day2 hour) desc)

selectP :: Parser Comm
selectP = do reserved lis "ver"
             date <- dateP
             return (Select date)

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