# dsl-eventos

## Comandos disponibles

### Agregar

##### _Agregar evento en una fecha y hora_
agregar [fecha] [hora] [descripción] <br/> **Por ejemplo:** agregar 01/01/2020 13:00 almuerzo de año nuevo; <br/>

##### _Agregar evento entre dos fechas a cierta hora_
agregar [fecha2] -r [fecha2] [hora] [descripción] <br/> **Por ejemplo:** agregar 05/01/2020 -r 10/01/2020 12:30 almuerzo de vacaciones; <br/>

##### _Agregar evento todos los días de un mes a cierta hora_
agregar todos/[mes]/[año] [hora] [descripción] <br/> **Por ejemplo:** agregar todos/02/2020 22:00 cena en febrero; <br/>

##### _Agregar el mismo evento todos los días específicos de un mes_
agregar [día_de_la_semana]/[mes]/[año] [hora] [descripción] <br/> **Por ejemplo:** agregar miercoles/10/2020 12:30 TODOS LOS MIERCOLES DE OCTUBRE; <br/>

### Modificar

##### _Modificar descripción_
modificar [fecha] [hora] -desc [descripción] <br/> **Por ejemplo:** modificar 28/10/2020 12:30 -desc todos los miercoles de octubre; <br/>

##### _Modificar fecha y hora_
modificar [fecha] [hora] -fd [fecha] [hora] <br/> **Por ejemplo:** modificar 21/10/2020 12:30 -fd 22/10/2020 13:30; <br/>

##### _Modificar fecha_
modificar [fecha] [hora] -d [fecha] <br/> **Por ejemplo:** modificar 22/10/2020 13:30 -d 21/10/2020; <br/>

##### _Modificar hora_
modificar [fecha] [hora] -t [hora] <br/> **Por ejemplo:** modificar 21/10/2020 13:30 -t 12:30; <br/>

### Cancelar

#### _Cancelar evento en una fecha y hora_
cancelar -fd [fecha] [hora] <br/> **Por ejemplo:** cancelar -fd 06/02/2020 22:00; <br/>

#### _Cancelar eventos en una fecha_
cancelar -d [fecha] <br/> **Por ejemplo:** cancelar -d 07/02/2020; <br/>

### Ver

#### _Ver eventos de una fecha_
ver [fecha] <br/> **Por ejemplo:** ver 28/10/2020; <br/>

#### _Ver un evento en una fecha y hora_
ver [fecha] [hora] <br/> **Por ejemplo:** ver 22/10/2020 13:30; <br/>

# Tipos de datos base de nuestro AST

$ ghci AST.hs

*AST> :t UTCTime <br/> UTCTime :: Day -> DiffTime -> UTCTime

Vemos que para formar un tipo de dato UTCTime necesitamos algo de tipo Day y algo de tipo DiffTime. Vamos a tomar a DiffTime como si fueran segundos.

Para formar algo del tipo Day usamos el método fromGregorian:
*AST> fromGregorian 2020 12 25 <br/> 2020-12-25

Vemos su tipo:
*AST> :t fromGregorian 2020 12 25 <br/> fromGregorian 2020 12 25 :: Day

Para formar algo del tipo UTCTime:
*AST> let day = fromGregorian 2020 12 25
*AST> let utc = UTCTime day (12*60*60 + 30*60)
*AST> utc <br/> 2020-12-25 12:30:00 UTC

# Cómo testear los parsers de los distintos comandos

$ ghci Parser.hs

*Parser> parse (totParser insertP) "" "agregar 01/01/2021 00:00 año nuevo" <br/> Right (Insert 2021-01-01 00:00:00 UTC "a\241o nuevo")

*Parser> parse (totParser insertBetweenP) "" "agregar 05/01/2021 -r 10/01/2021 12:30 almuerzo" <br/> Right (InsertBetween 2021-01-05 12:30:00 UTC 2021-01-10 12:30:00 UTC "almuerzo")

*Parser> parse (totParser selectP) "" "ver 05/01/2021 12:30" <br/> Right (Select 2021-01-05 12:30:00 UTC)

*Parser> parse (totParser dayP) "" "05/01/2020" <br/> Right 2020-01-05

*Parser> parse (totParser hourP) "" "12:30" <br/> Right 45000s

*Parser> parse (totParser dateP) "" "25/12/2020 12:30" <br/> Right 2020-12-25 12:30:00 UTC
