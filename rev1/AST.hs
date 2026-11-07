module AST where

-- Identificadores
type Id = String
type Variable = String

type Decllist = [Decl]
type IntExp = DoubleExp

-- <program> ::= 'declare' '{' <decllist> '}' ';' <asig> ';' <comm>
data Program = Program [Decl] [Asig] Comm deriving (Show, Eq)

-- <decllist> ::= <decl> ';' <decllist> | ε
-- (se representa directamente como lista de Decl en Program)

-- <decl> ::= 'conector' <id> | 'eslinga' <id> | 'carga' <id>
data Decl = DeclConector Id
          | DeclEslinga  Id
          | DeclCarga    Id
            deriving (Show, Eq)

-- <asig> ::= <id> '=' <tipo> ';' <asig> | ε
data Asig = Asig Id Tipo deriving (Show, Eq)

-- <tipo> ::= 'gancho' '(' <expr> ')'
--          | 'grillete' '(' <expr> ')'
--          | 'percha' '(' <expr> ')'
--          | 'cadena' '(' <expr> ')'
--          | 'sintetica' '(' <expr> ')'
--          | <expr>
--          | ε

data Tipo = Gancho    DoubleExp
          | Grillete  DoubleExp
          | Percha    DoubleExp
          | Cadena    DoubleExp
          | Sintetica DoubleExp
          | TipoExpr  DoubleExp
          | TipoVacio
            deriving (Show, Eq)

-- <comm> ::= 'connect' <gancho> '(' <ramificaciones> ')'
data Comm = Connect Variable [Rama]
          | Draw
          | Skip
          | Let Variable IntExp
          | Each [Variable] Tipo
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat Comm BoolExp
          deriving (Show, Eq)


data Rama = R Enlace Variable [Rama]
          | Rc Variable Variable
          deriving (Show, Eq)

data Enlace = En Variable
            | SinEslinga
            deriving (Show, Eq)

-- <dibujar>
-- data Dibujar = Dibujar deriving (Show, Eq)

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq DoubleExp DoubleExp
             | Lt DoubleExp DoubleExp
             | Gt DoubleExp DoubleExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
             deriving (Show, Eq)

-- Expresiones Aritmeticas
data DoubleExp = Const Double
            | Var Variable
            | UMinus DoubleExp
            | Plus DoubleExp DoubleExp
            | Minus DoubleExp DoubleExp
            | Times DoubleExp DoubleExp
            | Div DoubleExp DoubleExp
            deriving (Show, Eq)