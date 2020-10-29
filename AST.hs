module AST where

import Data.Time

type Descripcion = String
type NombreArchivo = String

data Comm = Insert UTCTime Descripcion -- agrega un evento en una fecha
            | InsertBetween UTCTime UTCTime Descripcion -- agrega el mismo evento entre 2 fechas
            | InsertAllDays UTCTime UTCTime Descripcion -- agrega el mismo evento todos los días de un mes
            | UpdateDescription UTCTime Descripcion
            | UpdateFullDate UTCTime UTCTime
            | UpdateDate UTCTime UTCTime
            | UpdateTime UTCTime UTCTime
            | CancelEventDate UTCTime
            | CancelEventDay UTCTime
            | Seq Comm Comm
            | SelectDate UTCTime
            | SelectFullDate UTCTime
            | Skip
 deriving Show

data FileComm = New NombreArchivo Comm
                | Open NombreArchivo Comm
 deriving Show