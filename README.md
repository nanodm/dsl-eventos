## dsl-eventos

# Tipos de datos base de nuestro AST

$ ghci AST.Hhs

*AST> :t UTCTime 
UTCTime :: Day -> DiffTime -> UTCTime

Vemos que para formar un tipo de dato UTCTime necesitamos algo de tipo Day y algo de tipo DiffTime. Vamos a tomar a DiffTime como si fueran segundos.

Para formar algo del tipo Day usamos el método fromGregorian:
*AST> fromGregorian 2020 12 25
2020-12-25

Vemos su tipo:
*AST> :t fromGregorian 2020 12 25
fromGregorian 2020 12 25 :: Day

Para formar algo del tipo UTCTime:
*AST> let day = fromGregorian 2020 12 25
*AST> let utc = UTCTime day (12*60*60 + 30*60)
*AST> utc
2020-12-25 12:30:00 UTC

# Cómo testear los parsers de los distintos comandos

$ ghci Parser.hs

*Parser> parse (totParser insertP) "" "agregar 01/01/2021 00:00 año nuevo"
Right (Insert 2021-01-01 00:00:00 UTC "a\241o nuevo")

*Parser> parse (totParser insertBetweenP) "" "agregar 05/01/2021 -r 10/01/2021 12:30 almuerzo"
Right (InsertBetween 2021-01-05 12:30:00 UTC 2021-01-10 12:30:00 UTC "almuerzo")

*Parser> parse (totParser selectP) "" "ver 05/01/2021 12:30"
Right (Select 2021-01-05 12:30:00 UTC)

*Parser> parse (totParser dayP) "" "05/01/2020"
Right 2020-01-05

*Parser> parse (totParser hourP) "" "12:30"
Right 45000s

*Parser> parse (totParser dateP) "" "25/12/2020 12:30"
Right 2020-12-25 12:30:00 UTC
