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
                                  , reservedNames = ["true","false","skip","if","then","else","while","repeat","until","end","declare","conector","eslinga","carga","gancho","grillete","percha","cadena","sintetica","connect","dibujar","null"]
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
                                                       , ":="
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
compopp = try (reservedOp dsl "=" >> return Eq)
      <|> try (reservedOp dsl "<" >> return Lt)
      <|> (reservedOp dsl ">" >> return Gt)

boolvalue :: Parser BoolExp
boolvalue = try (do reserved dsl "true"
                    return BTrue)
         <|> try (do reserved dsl "false"
                     return BFalse)

-----------------------------------
--- Parser de coma
-----------------------------------
comm :: Parser Comm
comm = chainl1 comm2 (try (do reservedOp dsl ";"
                              return Seq))

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
    <|> try (do str <- identifier dsl
                reservedOp dsl ":="
                e <- doubleexp
                return (Let str e))
    <|> try (do reserved dsl "connect"
                char '['
                c <- camino
                char ']'
                char ';'
                d <- dibujar
                return (Connect c d))

camino :: Parser Camino
camino = try (do nl1 <- nodolist
                 char ','
                 el <- eslingalist
                 char ','
                 nl2 <- nodolist
                 return (CaminoBase nl1 el nl2))
      <|> try (do nl1 <- nodolist
                  char ','
                  el <- eslingalist
                  char ','
                  c <- camino
                  return (CaminoPaso nl1 el c))

nodolist :: Parser NodoList
nodolist = do char '['
              ns <- sepBy nodo (char ',')
              char ']'
              return ns

nodo :: Parser Nodo
nodo = try (do str <- identifier dsl
               return (NodoId str))
   <|> try (do reserved dsl "null"
               return NodoNull)

eslingalist :: Parser EslingaList
eslingalist = do char '['
                 es <- sepBy eslinga (char ',')
                 char ']'
                 return es

eslinga :: Parser Eslinga
eslinga = try (do str <- identifier dsl
                  return (EslingaId str))
       <|> try (do reserved dsl "null"
                   return EslingaNull)

program :: Parser Program
program = do reserved dsl "declare"
             char '{'
             ds <- decllist
             char '}'
             reservedOp dsl ";"
             as <- many asig
             reservedOp dsl ";"
             c <- comm
             return (Program ds as c)

decllist :: Parser Decllist
decllist = sepBy decl (reservedOp dsl ";")

decl :: Parser Decl
decl =
      (do reserved dsl "conector"
          nombre <- identifier dsl
          return (DeclConector nombre))
  <|> (do reserved dsl "eslinga"
          nombre <- identifier dsl
          return (DeclEslinga nombre))
  <|> (do reserved dsl "carga"
          nombre <- identifier dsl
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


dibujar :: Parser Dibujar
dibujar = do reserved dsl "dibujar"
             return Dibujar

------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

parseProgram :: SourceName -> String -> Either ParseError Program
parseProgram = parse (totParser program)