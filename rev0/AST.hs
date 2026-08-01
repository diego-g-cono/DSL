module AST where

-- Identificadores
type Variable = String

-- Tipos del lenguaje
data Program = Program [Decl] [Comm]

data Type = TConector
          | TEslinga
          | TCarga
          | TInt
          | TDouble
          deriving (Show, Eq)

-- Declaraciones
data Decl = Decl Type Variable
          | DeclArray Type Variable Integer
        deriving (Show, Eq)

-- Expresiones
data ComponentKind = KGancho
                   | KGrillete
                   | KPercha
                   | KCancamo
                   | KCadena
                   | KSintetica
                deriving (Show, Eq)

data Exp = IntConst Integer
         | FloatConst Double
         | Var Variable
         | UMinus Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Prod Exp Exp
         | Div Exp Exp
         | Constructor ComponentKind Exp
        deriving (Show, Eq)

-- Expresiones booleanas
data BoolExp = BTrue
             | BFalse
             | Eq Exp Exp
             | Lt Exp Exp
             | Gt Exp Exp
             | And BoolExp BoolExp
             | Or  BoolExp BoolExp
             | Not BoolExp
            deriving (Show, Eq)

-- Segmento de conexión

data Segment = Segment { origen  :: Variable,
                         eslinga :: Variable,
                         destino :: Variable
                       }
            deriving (Show, Eq)

-- Comandos
data Comm = Let Variable Exp
          | ForEach { variableColeccion :: Variable
                    , coleccion :: Variable
                    , cuerpo :: [Comm]
                    }
          | Connect {segmentos :: [Segment]}
          | Draw
         deriving (Show, Eq)