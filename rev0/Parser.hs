module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)

import AST

--------------------------------------------------------
-- Lexer
--------------------------------------------------------

dsl :: TokenParser u
dsl = makeTokenParser $
    emptyDef
    {
        commentStart = "/*",
        commentEnd   = "*/",
        commentLine  = "//",

        reservedNames =
        [ "declare"
        , "connect"
        , "draw"
        , "foreach"
        , "in"

        , "conector"
        , "eslinga"
        , "carga"
        , "int"
        , "float"

        , "gancho"
        , "grillete"
        , "percha"
        , "cancamo"
        , "cadena"
        , "sintetica"

        , "true"
        , "false"
        ],

        reservedOpNames =
        [ "="
        , ";"
        , "->"
        , "+"
        , "-"
        , "*"
        , "/"
        , "=="
        , "<"
        , ">"
        , "&&"
        , "||"
        ]
    }

--------------------------------------------------------
-- Alias
--------------------------------------------------------

identifier' :: Parser String
identifier' = identifier dsl

reserved' :: String -> Parser ()
reserved' = reserved dsl

reservedOp' :: String -> Parser ()
reservedOp' = reservedOp dsl

integer' :: Parser Integer
integer' = integer dsl

float' :: Parser Double
float' = float dsl

semi' :: Parser String
semi' = semi dsl

comma' :: Parser String
comma' = comma dsl

whiteSpace' :: Parser ()
whiteSpace' = whiteSpace dsl

symbol' :: String -> Parser String
symbol' = symbol dsl

parens' :: Parser a -> Parser a
parens' = parens dsl

braces' :: Parser a -> Parser a
braces' = braces dsl

brackets' :: Parser a -> Parser a
brackets' = brackets dsl

--------------------------------------------------------
-- Parser completo
--------------------------------------------------------

totParser :: Parser a -> Parser a
totParser p = do
    whiteSpace'
    r <- p
    eof
    return r

--------------------------------------------------------
-- Parser de tipos
--------------------------------------------------------

typeParser :: Parser Type
typeParser =
        try (reserved' "conector" >> return TConector)
    <|> try (reserved' "eslinga"  >> return TEslinga)
    <|> try (reserved' "carga"    >> return TCarga)
    <|> try (reserved' "int"      >> return TInt)
    <|> try (reserved' "float"    >> return TFloat)

--------------------------------------------------------
-- Parser de expresiones
--------------------------------------------------------

expParser :: Parser Exp
expParser = chainl1 term addOp

term :: Parser Exp
term = chainl1 factor multOp

factor :: Parser Exp
factor =
        try (parens' expParser)

    <|> try constructorParser

    <|> try (do
            reservedOp' "-"
            e <- factor
            return (UMinus e))

    <|> try (do
            n <- float'
            return (FloatConst n))

    <|> try (do
            n <- integer'
            return (IntConst n))

    <|> do
            v <- identifier'
            return (Var v)

--------------------------------------------------------
-- Constructores del dominio
--------------------------------------------------------

constructorParser :: Parser Exp

constructorParser =
        try (constructor "gancho" Gancho)
    <|> try (constructor "grillete" Grillete)
    <|> try (constructor "percha" Percha)
    <|> try (constructor "cancamo" Cancamo)
    <|> try (constructor "cadena" Cadena)
    <|> try (constructor "sintetica" Sintetica)

constructor :: String -> (Exp -> Exp) -> Parser Exp
constructor nombre ctor = do
    reserved' nombre
    e <- parens' expParser
    return (ctor e)

--------------------------------------------------------
-- Operadores aritméticos
--------------------------------------------------------

multOp :: Parser (Exp -> Exp -> Exp)

multOp =
        try (do
            reservedOp' "*"
            return Times)

    <|> try (do
            reservedOp' "/"
            return Div)

addOp :: Parser (Exp -> Exp -> Exp)

addOp =
        try (do
            reservedOp' "+"
            return Plus)

    <|> try (do
            reservedOp' "-"
            return Minus)

--------------------------------------------------------
-- Parser de expresiones booleanas
--------------------------------------------------------

boolParser :: Parser BoolExp
boolParser = chainl1 boolTerm boolOr

boolTerm :: Parser BoolExp
boolTerm = chainl1 boolFactor boolAnd

boolFactor :: Parser BoolExp

boolFactor =
        try (parens' boolParser)

    <|> try (do
            reserved' "true"
            return BTrue)

    <|> try (do
            reserved' "false"
            return BFalse)

    <|> comparison

comparison :: Parser BoolExp
comparison = do

    e1 <- expParser

    op <-
            try (reservedOp' "==" >> return Eq)
        <|> try (reservedOp' "<"  >> return Lt)
        <|> try (reservedOp' ">"  >> return Gt)

    e2 <- expParser

    return (op e1 e2)

boolAnd :: Parser (BoolExp -> BoolExp -> BoolExp)

boolAnd = do
    reservedOp' "&&"
    return And

boolOr :: Parser (BoolExp -> BoolExp -> BoolExp)

boolOr = do
    reservedOp' "||"
    return Or

--------------------------------------------------------
-- Declaraciones
--------------------------------------------------------

declParser :: Parser Decl
declParser = try declArrayParser
         <|> declSimpleParser

--------------------------------------------------------

declSimpleParser :: Parser Decl
declSimpleParser = do

    t <- typeParser
    v <- identifier'
    semi'

    return (Decl t v)

--------------------------------------------------------

declArrayParser :: Parser Decl
declArrayParser = do

    t <- typeParser
    v <- identifier'

    n <- brackets' integer'

    semi'

    return (DeclArray t v n)

--------------------------------------------------------

declList :: Parser [Decl]
declList = many declParser

--------------------------------------------------------
-- Parser de un segmento
--
-- g1 -> e1 -> p1
--------------------------------------------------------

segmentParser :: Parser Segment
segmentParser = do

    n1 <- identifier'

    reservedOp' "->"

    e <- identifier'

    reservedOp' "->"

    n2 <- identifier'

    return (Segment n1 e n2)

--------------------------------------------------------
-- Lista de segmentos
--------------------------------------------------------

segmentList :: Parser [Segment]

segmentList = do

    s <- segmentParser

    ss <- many
        (try (do

            reservedOp' "->"

            e <- identifier'

            reservedOp' "->"

            n <- identifier'

            case s of
                Segment _ _ ultimo ->
                    return (Segment ultimo e n)
        ))

    return (s:ss)

--------------------------------------------------------
-- Asignaciones
--------------------------------------------------------

letParser :: Parser Comm
letParser = do

    v <- identifier'

    reservedOp' "="

    e <- expParser

    semi'

    return (Let v e)

--------------------------------------------------------
-- foreach
--------------------------------------------------------

foreachParser :: Parser Comm
foreachParser = do

    reserved' "foreach"

    var <- identifier'

    reserved' "in"

    coleccion <- identifier'

    cuerpo <- braces' commList

    return (ForEach var coleccion cuerpo)

--------------------------------------------------------
-- connect
--------------------------------------------------------

connectParser :: Parser Comm
connectParser = do

    reserved' "connect"

    segs <- parens' segmentList

    semi'

    return (Connect segs)

--------------------------------------------------------
-- draw
--------------------------------------------------------

drawParser :: Parser Comm
drawParser = do

    reserved' "draw"

    semi'

    return Draw

--------------------------------------------------------
-- Un comando
--------------------------------------------------------

commParser :: Parser Comm

commParser =
        try foreachParser
    <|> try connectParser
    <|> try drawParser
    <|> try letParser

--------------------------------------------------------
-- Lista de comandos
--------------------------------------------------------

commList :: Parser [Comm]

commList = many commParser

--------------------------------------------------------
-- Bloque de comandos
--------------------------------------------------------

blockParser :: Parser Comm
blockParser = do
    cs <- braces' commList
    return (Seq cs)

--------------------------------------------------------
-- foreach
--------------------------------------------------------

foreachParser :: Parser Comm
foreachParser = do

    reserved' "foreach"

    v <- identifier'

    reserved' "in"

    arr <- identifier'

    body <- blockParser

    return (ForEach v arr body)

--------------------------------------------------------
-- connect
--------------------------------------------------------

connectParser :: Parser Comm
connectParser = do

    reserved' "connect"

    segs <- parens' segmentList

    semi'

    return (Connect segs)

--------------------------------------------------------
-- draw
--------------------------------------------------------

drawParser :: Parser Comm
drawParser = do

    reserved' "draw"

    semi'

    return Draw

--------------------------------------------------------
-- Asignación
--------------------------------------------------------

letParser :: Parser Comm
letParser = do

    v <- identifier'

    reservedOp' "="

    e <- expParser

    semi'

    return (Let v e)

--------------------------------------------------------
-- Un comando
--------------------------------------------------------

commParser :: Parser Comm

commParser =
        try foreachParser
    <|> try connectParser
    <|> try drawParser
    <|> try letParser

--------------------------------------------------------
-- Lista de comandos
--------------------------------------------------------

commList :: Parser [Comm]
commList = many commParser