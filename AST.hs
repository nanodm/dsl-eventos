module AST where

import Data.Dates
import Data.Time

type Descripcion = String
type NombreArchivo = String

-- data Evento = Evento DateTime Descripcion deriving Show

-- Comandos para crear o abrir archivos
-- data FileComm = New NombreArchivo
--                | Open NombreArchivo
--                deriving Show

-- Comandos para agregar a un archivo
data Comm = Add1 UTCTime Descripcion -- agrega un evento en una fecha
            | Add2 UTCTime UTCTime Descripcion -- agrega el mismo evento entre 2 fechas
            | Seq Comm Comm
            | Skip
 deriving Show