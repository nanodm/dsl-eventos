module AST where

import Data.Dates
import Data.Time

type Descripcion = String
type NombreArchivo = String

-- Comandos para agregar a un archivo
data Comm = Add UTCTime Descripcion -- agrega un evento en una fecha
            | AddBetween UTCTime UTCTime Descripcion -- agrega el mismo evento entre 2 fechas
            | Seq Comm Comm
            | Skip
 deriving Show