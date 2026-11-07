module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace dsl
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
dsl :: TokenParser u
dsl = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , reservedNames = ["true",
                                                     "false",
                                                     "skip",
                                                     "if",
                                                     "then",
                                                     "else",
                                                     "while",
                                                     "repeat",
                                                     "until",
                                                     "end",
                                                     "declare",
                                                     "conector",
                                                     "eslinga",
                                                     "carga",
                                                     "gancho",
                                                     "grillete",
                                                     "percha",
                                                     "cadena",
                                                     "sintetica",
                                                     "each",
                                                     "are",
                                                     "connect",
                                                     "draw",
                                                     "dibujar",
                                                     "null"]
                                  , reservedOpNames = [  "+"
                                                       , "-"
                                                       , "*"
                                                       , "/"
                                                       , "<"
                                                       , ">"
                                                       , "&"
                                                       , "|"
                                                       , "="
                                                       , ";"
                                                       , "=="
                                                       ]
                                   }
                                 )

-----------------------------------
--- Parser de expresiones aritmeticas
-----------------------------------
doubleexp :: Parser DoubleExp
doubleexp = chainl1 term addopp

term :: Parser DoubleExp
term = chainl1 factor multopp

factor :: Parser DoubleExp
factor = try (parens dsl doubleexp)
      <|> try (do reservedOp dsl "-"
                  f <- factor
                  return (UMinus f))
      <|> try (do n <- integer dsl
                  return (Const (fromIntegral n)))
      <|> do str <- identifier dsl
             return (Var str)

multopp :: Parser (DoubleExp -> DoubleExp -> DoubleExp)
multopp = try (reservedOp dsl "*" >> return Times)
      <|> (reservedOp dsl "/" >> return Div)

addopp :: Parser (DoubleExp -> DoubleExp -> DoubleExp)
addopp = try (reservedOp dsl "+" >> return Plus)
      <|> (reservedOp dsl "-" >> return Minus)

-----------------------------------
--- Parser de expresiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp = chainl1 boolexp2 (try (do reservedOp dsl "|"
                                    return Or))

boolexp2 :: Parser BoolExp
boolexp2 = chainl1 boolexp3 (try (do reservedOp dsl "&"
                                     return And))

boolexp3 :: Parser BoolExp
boolexp3 = try (do i <- doubleexp
                   op <- compopp
                   j <- doubleexp
                   return (op i j))
        <|> boolvalue
        <|> parens dsl boolexp

compopp :: Parser (DoubleExp -> DoubleExp -> BoolExp)
compopp = try (do reservedOp dsl "==" 
                  return Eq)
      <|> try (do reservedOp dsl "<" 
                  return Lt)
      <|> (do reservedOp dsl ">" 
              return Gt)

boolvalue :: Parser BoolExp
boolvalue = try (do reserved dsl "true"
                    return BTrue)
         <|> try (do reserved dsl "false"
                     return BFalse)

-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: Parser Comm
comm = do c <- comm2
          cs <- many (try (do reservedOp dsl ";"
                              c2 <- comm2
                              return c2))
          optional (reservedOp dsl ";")
          return (foldl Seq c cs)

comm2 :: Parser Comm
comm2 = try (do reserved dsl "skip"
                return Skip)
    <|> try (do reserved dsl "if"
                c <- boolexp
                reserved dsl "then"
                t <- comm
                reserved dsl "else"
                e <- comm
                reserved dsl "end"
                return (Cond c t e))
    <|> try (do reserved dsl "repeat"
                body <- comm
                reserved dsl "until"
                cond <- boolexp
                reserved dsl "end"
                return (Repeat body cond))
    <|> try (do reserved dsl "draw"
                return Draw)
    <|> try (do reserved dsl "each"
                vars <- sepBy1 (identifier dsl) (do spaces; char ','; spaces)
                reserved dsl "are"
                t <- tipo
                return (Each vars t))
    <|> try (do str <- identifier dsl
                reservedOp dsl "="
                e <- doubleexp
                return (Let str e))
    <|> try (do reserved dsl "connect"
                g <- identifier dsl
                symbol dsl "("
                rs <- sepBy1 rama (symbol dsl ",")
                symbol dsl ")"
                return (Connect g rs))

enlace :: Parser Enlace
enlace = try (do reserved dsl "null"
                 return SinEslinga)
     <|> do e <- identifier dsl
            return (En e)

rama :: Parser Rama
rama = try ramaIntermedia
   <|> try ramaHoja

ramaIntermedia :: Parser Rama
ramaIntermedia = do e <- enlace
                    n <- identifier dsl
                    symbol dsl "("
                    rs <- sepBy1 rama (symbol dsl ",")
                    symbol dsl ")"
                    return (R e n rs)

ramaHoja :: Parser Rama
ramaHoja = try (do v1 <- identifier dsl
                   symbol dsl ","
                   v2 <- identifier dsl
                   return (Rc v1 v2))
            <|> do v1 <- identifier dsl
                   v2 <- identifier dsl
                   return (Rc v1 v2)

program :: Parser Program
program = do
    reserved dsl "declare"
    symbol dsl "{"
    ds <- decllist
    symbol dsl "}"
    symbol dsl ";"
    as <- many asigOrEach
    c <- comm
    return (Program ds (concat as) c)

asigOrEach :: Parser [Asig]
asigOrEach =
    try asigSimple
    <|> asigEach

asigSimple :: Parser [Asig]
asigSimple = do
    a <- asig
    return [a]

asigEach :: Parser [Asig]
asigEach = do
    reserved dsl "each"
    vars <- sepBy1 (identifier dsl) (symbol dsl ",")
    reserved dsl "are"
    t <- tipo
    symbol dsl ";"
    return (makeAsig vars t)

makeAsig :: [Variable] -> Tipo -> [Asig]
makeAsig vars t = map make vars
    where
        make v = Asig v t

decllist :: Parser Decllist
decllist = do ds <- many decl
              return ds

decl :: Parser Decl
decl =
      (do reserved dsl "conector"
          nombre <- identifier dsl
          reservedOp dsl ";"
          return (DeclConector nombre))
  <|> (do reserved dsl "eslinga"
          nombre <- identifier dsl
          reservedOp dsl ";"
          return (DeclEslinga nombre))
  <|> (do reserved dsl "carga"
          nombre <- identifier dsl
          reservedOp dsl ";"
          return (DeclCarga nombre))

asig :: Parser Asig
asig = do nombre <- identifier dsl
          reservedOp dsl "="
          t <- tipo
          reservedOp dsl ";"
          return (Asig nombre t)

tipo :: Parser Tipo
tipo = try (do reserved dsl "gancho"
               char '('
               e <- doubleexp
               char ')'
               return (Gancho e))
   <|> try (do reserved dsl "grillete"
               char '('
               e <- doubleexp
               char ')'
               return (Grillete e))
   <|> try (do reserved dsl "percha"
               char '('
               e <- doubleexp
               char ')'
               return (Percha e))
   <|> try (do reserved dsl "cadena"
               char '('
               e <- doubleexp
               char ')'
               return (Cadena e))
   <|> try (do reserved dsl "sintetica"
               char '('
               e <- doubleexp
               char ')'
               return (Sintetica e))
   <|> try (do e <- doubleexp
               return (TipoExpr e))
   <|> return TipoVacio

------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

parseProgram :: SourceName -> String -> Either ParseError Program
parseProgram = parse (totParser program)