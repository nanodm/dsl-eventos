module AST where

import Data.Time

type Description = String
type FileName = String

data Comm = Insert UTCTime Description -- agrega un evento en una fecha
            | InsertBetween UTCTime UTCTime Description -- agrega el mismo evento entre 2 fechas
            | InsertAllDays UTCTime UTCTime Description -- agrega el mismo evento todos los días de un mes
            | InsertWeekly UTCTime UTCTime Description String -- agrega el mismo evento todos los días específicos de un mes
            | InsertFullDay UTCTime Description -- agrega un evento todo el día
            | UpdateDescription UTCTime Description
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

data FileComm = New FileName Comm
                | Open FileName Comm
 deriving Show