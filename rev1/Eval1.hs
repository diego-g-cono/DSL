module Eval1 (eval, evalProgram, renderGraph, validateProgram) where

import AST
import Data.List (intercalate, nub)
import System.IO (writeFile)
import System.IO.Unsafe (unsafePerformIO)

-- Estado para valores numericos
type State = [(Variable, Double)]
type TypeMap = [(Variable, Tipo)]

initState :: State
initState = []

lookupType :: Variable -> TypeMap -> Maybe Tipo
lookupType = lookup

isPerchaType :: Variable -> TypeMap -> Bool
isPerchaType var typeMap = case lookupType var typeMap of
                                Just (Percha _) -> True
                                _ -> False

isGanchoOrGrilleteType :: Variable -> TypeMap -> Bool
isGanchoOrGrilleteType var typeMap = case lookupType var typeMap of
                                          Just (Gancho _) -> True
                                          Just (Grillete _) -> True
                                          _ -> False

isEslingaType :: Tipo -> Bool
isEslingaType (Cadena _) = True
isEslingaType (Sintetica _) = True
isEslingaType _ = False

isConectorType :: Tipo -> Bool
isConectorType (Gancho _) = True
isConectorType (Grillete _) = True
isConectorType (Percha _) = True
isConectorType _ = False

lookfor :: Variable -> State -> Double
lookfor var [] = 0
lookfor var ((x, y):xs)
  | var == x = y
  | otherwise = lookfor var xs

update :: Variable -> Double -> State -> State
update var valor [] = [(var, valor)]
update var valor ((x, y):xs)
  | var == x = (var, valor):xs
  | otherwise = (x, y) : update var valor xs

-- Ejecuta un programa completo validando primero la estructura y luego evaluando
-- sus asignaciones y el comando principal.
evalProgram :: Program -> State
evalProgram program = case validateProgram program of
  Left err -> error err
  Right (Program _ asigs comm) ->
    let st0 = evalAsigs asigs initState
        typeMap = makeTypeMap asigs
        (finalState, _) = evalCommWithTypeMap comm st0 typeMap
    in finalState

-- Aplica secuencialmente una lista de asignaciones sobre un estado inicial.
evalAsigs :: [Asig] -> State -> State
evalAsigs [] st = st
evalAsigs (a:as) st = evalAsigs as (evalAsig st a)

-- Construye un mapa de tipos a partir de las asignaciones declaradas.
makeTypeMap :: [Asig] -> TypeMap
makeTypeMap [] = []
makeTypeMap (Asig var tipo : as) = (var, tipo) : makeTypeMap as

-- Evalúa una asignación individual y devuelve el estado actualizado.
evalAsig :: State -> Asig -> State
evalAsig st (Asig var tipo) = update var (evalTipo tipo st) st

-- Valida que el programa sea consistente.
validateProgram :: Program -> Either String Program
validateProgram p@(Program decls asigs comm) =
  let declNames = map declName decls
      declMap = map (\d -> (declName d, d)) decls
      duplicateNames = findDuplicates declNames
      state = evalAsigs asigs initState
      typeMap = asigTypeMap state asigs
  in if not (null duplicateNames)
      then Left ("Hay declaraciones duplicadas: " ++ intercalate ", " duplicateNames)
      else do
        validateAsigs declMap asigs
        validateAsigVars declNames asigs
        _ <- validateCommWithTypeMap declNames declMap typeMap comm
        return p

asigTypeMap :: State -> [Asig] -> TypeMap
asigTypeMap _ [] = []
asigTypeMap state (Asig var tipo:as) = (var, resolveTipo state tipo) : asigTypeMap state as

validateAsigs :: [(String, Decl)] -> [Asig] -> Either String ()
validateAsigs _ [] = Right ()
validateAsigs declMap (a:as) = do
  validateAsigType declMap a
  validateAsigs declMap as

validateAsigVars :: [String] -> [Asig] -> Either String ()
validateAsigVars _ [] = Right ()
validateAsigVars declNames (Asig var _:as) = do
  validateVariable declNames var
  validateAsigVars declNames as

findDuplicates :: [String] -> [String]
findDuplicates [] = []
findDuplicates (x:xs)
  | x `elem` xs = x : findDuplicates (filter (/= x) xs)
  | otherwise = findDuplicates xs

declName :: Decl -> String
declName (DeclConector x) = x
declName (DeclEslinga x) = x
declName (DeclCarga x) = x

validateVariable :: [String] -> String -> Either String ()
validateVariable declNames var =
  if var `elem` declNames
    then Right ()
    else Left ("La variable '" ++ var ++ "' debe declararse antes de usarse.")

resolveTipo :: State -> Tipo -> Tipo
resolveTipo st (Gancho e) = Gancho (resolveDoubleExp st e)
resolveTipo st (Grillete e) = Grillete (resolveDoubleExp st e)
resolveTipo st (Percha e) = Percha (resolveDoubleExp st e)
resolveTipo st (Cadena e) = Cadena (resolveDoubleExp st e)
resolveTipo st (Sintetica e) = Sintetica (resolveDoubleExp st e)
resolveTipo st (TipoExpr e) = TipoExpr (resolveDoubleExp st e)
resolveTipo _ TipoVacio = TipoVacio

resolveDoubleExp :: State -> DoubleExp -> DoubleExp
resolveDoubleExp _ (Const valor) = Const valor
resolveDoubleExp st (Var variable) = Const (lookfor variable st)
resolveDoubleExp st (UMinus e) = UMinus (resolveDoubleExp st e)
resolveDoubleExp st (Plus e1 e2) = Plus (resolveDoubleExp st e1) (resolveDoubleExp st e2)
resolveDoubleExp st (Minus e1 e2) = Minus (resolveDoubleExp st e1) (resolveDoubleExp st e2)
resolveDoubleExp st (Times e1 e2) = Times (resolveDoubleExp st e1) (resolveDoubleExp st e2)
resolveDoubleExp st (Div e1 e2) = Div (resolveDoubleExp st e1) (resolveDoubleExp st e2)

validateAsigType :: [(String, Decl)] -> Asig -> Either String ()
validateAsigType declMap (Asig var tipo) =
  case lookup var declMap of
    Just (DeclEslinga _) ->
      case tipo of
        Cadena _ -> Right ()
        Sintetica _ -> Right ()
        _ -> Left ("La variable '" ++ var ++ "' está declarada como eslinga y solo puede tener tipo cadena o sintetica.")
    Just (DeclConector _) ->
      case tipo of
        Gancho _ -> Right ()
        Grillete _ -> Right ()
        Percha _ -> Right ()
        _ -> Left ("La variable '" ++ var ++ "' está declarada como conector y solo puede tener tipo gancho, grillete o percha.")
    Just (DeclCarga _) -> Right ()
    Nothing -> Left ("La variable '" ++ var ++ "' debe declararse antes de usarse.")

validateComm :: [String] -> [(String, Decl)] -> TypeMap -> Comm -> Either String ()
validateComm declNames declMap typeMap comm = do
  _ <- validateCommWithTypeMap declNames declMap typeMap comm
  return ()

validateCommWithTypeMap :: [String] -> [(String, Decl)] -> TypeMap -> Comm -> Either String TypeMap
validateCommWithTypeMap declNames declMap typeMap Skip = Right typeMap
validateCommWithTypeMap declNames declMap typeMap Draw = Right typeMap
validateCommWithTypeMap declNames declMap typeMap (Let var exp) = do
  validateVariable declNames var
  validateDoubleExp declNames exp
  Right typeMap
validateCommWithTypeMap declNames declMap typeMap (Each vars tipo) = do
  validateEachVariables declNames vars
  validateEachAsigns declMap vars tipo
  let eachState = evalAsigs (map (\var -> Asig var tipo) vars) initState
      resolvedTipo = resolveTipo eachState tipo
  Right (updateEachTypeInValidation vars resolvedTipo typeMap)
validateCommWithTypeMap declNames declMap typeMap (Seq c1 c2) = do
  typeMap1 <- validateCommWithTypeMap declNames declMap typeMap c1
  validateCommWithTypeMap declNames declMap typeMap1 c2
validateCommWithTypeMap declNames declMap typeMap (Cond b c1 c2) = do
  validateBoolExp declNames b
  validateComm declNames declMap typeMap c1
  validateComm declNames declMap typeMap c2
  Right typeMap
validateCommWithTypeMap declNames declMap typeMap (Repeat c b) = do
  validateComm declNames declMap typeMap c
  validateBoolExp declNames b
  Right typeMap
validateCommWithTypeMap declNames declMap typeMap (Connect g ramas) = do
  validateConnect declNames declMap typeMap g ramas
  Right typeMap

validateEachVariables :: [String] -> [Variable] -> Either String ()
validateEachVariables _ [] = Right ()
validateEachVariables declNames (var:vars) = do
  validateVariable declNames var
  validateEachVariables declNames vars

validateEachAsigns :: [(String, Decl)] -> [Variable] -> Tipo -> Either String ()
validateEachAsigns _ [] _ = Right ()
validateEachAsigns declMap (var:vars) tipo = do
  validateAsigType declMap (Asig var tipo)
  validateEachAsigns declMap vars tipo

updateEachTypeInValidation :: [Variable] -> Tipo -> TypeMap -> TypeMap
updateEachTypeInValidation [] _ typeMap = typeMap
updateEachTypeInValidation (var:vars) tipo typeMap =
  updateEachTypeInValidation vars tipo (updateTypeMap var tipo typeMap)

updateTypeMap :: Variable -> Tipo -> TypeMap -> TypeMap
updateTypeMap var tipo [] = [(var, tipo)]
updateTypeMap var tipo ((x, y):xs)
  | var == x = (var, tipo):xs
  | otherwise = (x, y) : updateTypeMap var tipo xs

validateConnect :: [String] -> [(String, Decl)] -> TypeMap -> Variable -> [Rama] -> Either String ()
validateConnect declNames declMap typeMap g ramas = do
  validateVariable declNames g
  validateRamas declMap declNames typeMap ramas

validateRama :: [(String, Decl)] -> [String] -> TypeMap -> Rama -> Either String ()
validateRama declMap declNames typeMap (Rc a b) = do
  validateVariable declNames a
  validateVariable declNames b
  case lookupType a typeMap of
    Just t | isEslingaType t ->
      case lookup b declMap of
        Just (DeclCarga _) -> Right ()
        _ -> case lookupType b typeMap of
               Just t2 | isCargaTipo t2 -> Right ()
               _ -> Left "La rama hoja debe tener el último nodo con tipo de carga."
    _ -> Left "La rama hoja debe empezar con un nodo de tipo eslinga."
validateRama declMap declNames typeMap (R enlace p rs) = do
  case enlace of
    SinEslinga ->
      case lookupType p typeMap of
        Just t | isPerchaTipo t -> validateRamas declMap declNames typeMap rs
        _ -> Left "Cuando no hay eslinga, el siguiente nodo debe ser de tipo percha."
    En e -> do
      validateVariable declNames e
      case lookupType e typeMap of
        Just t | isEslingaType t ->
          case lookupType p typeMap of
            Just t2 | isConectorType t2 -> validateRamas declMap declNames typeMap rs
            _ -> Left "La rama intermedia debe continuar con un nodo de tipo conector."
        _ -> Left "La rama intermedia debe empezar en un nodo de tipo eslinga."

isCargaTipo :: Tipo -> Bool
isCargaTipo (Cadena _) = False
isCargaTipo (Sintetica _) = False
isCargaTipo (Gancho _) = False
isCargaTipo (Grillete _) = False
isCargaTipo (Percha _) = False
isCargaTipo (TipoExpr _) = True
isCargaTipo TipoVacio = False

isPerchaTipo :: Tipo -> Bool
isPerchaTipo (Percha _) = True
isPerchaTipo _ = False

validateRamas :: [(String, Decl)] -> [String] -> TypeMap -> [Rama] -> Either String ()
validateRamas _ _ _ [] = Right ()
validateRamas declMap declNames typeMap (r:rs) = do
  validateRama declMap declNames typeMap r
  validateRamas declMap declNames typeMap rs

-- Evalúa el valor numérico de un tipo concreto con el estado actual del programa.
evalTipo :: Tipo -> State -> Double
evalTipo (Gancho e) st = evalIntExp e st
evalTipo (Grillete e) st = evalIntExp e st
evalTipo (Percha e) st = evalIntExp e st
evalTipo (Cadena e) st = evalIntExp e st
evalTipo (Sintetica e) st = evalIntExp e st
evalTipo (TipoExpr e) st = evalIntExp e st
evalTipo TipoVacio _ = 0

eval :: Comm -> State
eval p = evalComm p initState []

evalComm :: Comm -> State -> TypeMap -> State
evalComm comm s typeMap = fst (evalCommWithTypeMap comm s typeMap)

evalCommWithTypeMap :: Comm -> State -> TypeMap -> (State, TypeMap)
evalCommWithTypeMap Skip s typeMap = (s, typeMap)
evalCommWithTypeMap Draw s typeMap = (s, typeMap)
evalCommWithTypeMap (Let var expInt) s typeMap = (update var (evalIntExp expInt s) s, typeMap)
evalCommWithTypeMap (Each vars tipo) s typeMap =
  let newState = evalEach vars tipo s
      newTypeMap = updateEachType vars tipo s typeMap
  in (newState, newTypeMap)
evalCommWithTypeMap (Seq Skip c1) s typeMap = evalCommWithTypeMap c1 s typeMap
evalCommWithTypeMap (Seq c0 c1) s typeMap =
  let (s1, typeMap1) = evalCommWithTypeMap c0 s typeMap
  in evalCommWithTypeMap c1 s1 typeMap1
evalCommWithTypeMap (Cond b c0 c1) s typeMap =
  if evalBoolExp b s then evalCommWithTypeMap c0 s typeMap else evalCommWithTypeMap c1 s typeMap
evalCommWithTypeMap (Repeat c b) s typeMap =
  if evalBoolExp b s then evalCommWithTypeMap (Seq c (Repeat c b)) s typeMap else (s, typeMap)
evalCommWithTypeMap (Connect g ramas) s typeMap =
  unsafePerformIO (writeFile "izaje.tex" (renderGraphWithTypeMap typeMap (Connect g ramas)) >> return (s, typeMap))

evalEach :: [Variable] -> Tipo -> State -> State
evalEach [] _ s = s
evalEach (var:vars) tipo s = evalEach vars tipo (update var (evalTipo tipo s) s)

updateEachType :: [Variable] -> Tipo -> State -> TypeMap -> TypeMap
updateEachType [] _ _ typeMap = typeMap
updateEachType (var:vars) tipo s typeMap =
  updateEachType vars tipo s (updateTypeMap var (resolveTipo s tipo) typeMap)

evalIntExp :: IntExp -> State -> Double
evalIntExp (Const valor) _ = valor
evalIntExp (Var variable) estado = lookfor variable estado
evalIntExp (UMinus expInt) estado = -(evalIntExp expInt estado)
evalIntExp (Plus exp1 exp2) estado = evalIntExp exp1 estado + evalIntExp exp2 estado
evalIntExp (Minus exp1 exp2) estado = evalIntExp exp1 estado - evalIntExp exp2 estado
evalIntExp (Times exp1 exp2) estado = evalIntExp exp1 estado * evalIntExp exp2 estado
evalIntExp (Div exp1 exp2) estado = evalIntExp exp1 estado / evalIntExp exp2 estado

evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue _ = True
evalBoolExp BFalse _ = False
evalBoolExp (Eq exp1 exp2) estado = evalIntExp exp1 estado == evalIntExp exp2 estado
evalBoolExp (Lt exp1 exp2) estado = evalIntExp exp1 estado < evalIntExp exp2 estado
evalBoolExp (Gt exp1 exp2) estado = evalIntExp exp1 estado > evalIntExp exp2 estado
evalBoolExp (And exp1 exp2) estado = evalBoolExp exp1 estado && evalBoolExp exp2 estado
evalBoolExp (Or exp1 exp2) estado = evalBoolExp exp1 estado || evalBoolExp exp2 estado
evalBoolExp (Not exp1) estado = not (evalBoolExp exp1 estado)

renderGraph :: Comm -> String
renderGraph = renderGraphWithTypeMap []

renderGraphWithTypeMap :: TypeMap -> Comm -> String
renderGraphWithTypeMap typeMap (Connect g ramas) =
  let edges = collectEdges g ramas ++ collectSameLevelPerchaEdges typeMap ramas
      nodes = nub (g : concatMap nodeVarsFromRama ramas)
      positions = treeLayout nodes g ramas
      nodeLines = map renderVertex positions
      edgeLines = map renderEdge edges
  in unlines (["\\documentclass[a4paper,12pt]{article}",
              "",
              "% Codificación y fuentes",
              "\\usepackage[utf8]{inputenc}",
              "\\usepackage[T1]{fontenc}",
              "",
              "% Gráficos",
              "\\usepackage{tikz}",
              "\\usepackage{tkz-graph}",
              "",
              "\\begin{document}",
              "",
              "\\begin{tikzpicture}",
              "    \\GraphInit[vstyle=Normal]",
              "    \\SetGraphUnit{4}"]
             ++ nodeLines
             ++ edgeLines
             ++ ["\\end{tikzpicture}",
                 "",
                 "\\end{document}"])
renderGraphWithTypeMap _ _ = unlines ["\\documentclass[a4paper,12pt]{article}",
                                    "\\usepackage[utf8]{inputenc}",
                                    "\\usepackage[T1]{fontenc}",
                                    "\\usepackage{tikz}",
                                    "\\usepackage{tkz-graph}",
                                    "\\begin{document}",
                                    "\\begin{tikzpicture}",
                                    "\\GraphInit[vstyle=Normal]",
                                    "\\SetGraphUnit{4}",
                                    "\\end{tikzpicture}",
                                    "\\end{document}"]

renderVertex :: (Variable, (Double, Double)) -> String
renderVertex (v, (x, y)) = "    \\Vertex[x=" ++ formatCoord x ++ ",y=" ++ formatCoord y ++ "]{" ++ v ++ "}"

formatCoord :: Double -> String
formatCoord value =
  let rounded = round value :: Int
  in if fromIntegral rounded == value then show rounded else show value

collectEdges :: Variable -> [Rama] -> [(Variable, Variable, Maybe Variable)]
collectEdges root ramas = nub (concatMap (collectEdgesFrom root) ramas)

collectSameLevelPerchaEdges :: TypeMap -> [Rama] -> [(Variable, Variable, Maybe Variable)]
collectSameLevelPerchaEdges typeMap ramas =
  concatMap (\nodes -> case filterPerchaNodes typeMap nodes of
                        [] -> []
                        _ -> zipWith (\a b -> (a, b, Nothing)) (filterPerchaNodes typeMap nodes) (tail (filterPerchaNodes typeMap nodes))) (perchaLevelsFrom typeMap ramas)

collectEdgesFrom :: Variable -> Rama -> [(Variable, Variable, Maybe Variable)]
collectEdgesFrom parent (R (En e) p rs) =
  (parent, p, Just e) : concatMap (collectEdgesFrom p) rs
collectEdgesFrom parent (R SinEslinga p rs) =
  concatMap (collectEdgesFrom p) rs
collectEdgesFrom parent (Rc a b) = [(parent, b, Just a)]

nodeVarsFromRama :: Rama -> [Variable]
nodeVarsFromRama (R _ p rs) = p : concatMap nodeVarsFromRama rs
nodeVarsFromRama (Rc _ b) = [b]

nodeLevelsFrom :: [Rama] -> [[Variable]]
nodeLevelsFrom branches =
  let first = directNodes branches
      rest = collectLevelChildren branches
  in first : rest

perchaLevelsFrom :: TypeMap -> [Rama] -> [[Variable]]
perchaLevelsFrom typeMap branches =
  let first = directPerchaNodes typeMap branches
      rest = collectPerchaLevelChildren typeMap branches
  in first : rest

collectLevelChildren :: [Rama] -> [[Variable]]
collectLevelChildren [] = []
collectLevelChildren branches =
  let children = concatMap childBranches branches
  in if null children then [] else directNodes children : collectLevelChildren children

collectPerchaLevelChildren :: TypeMap -> [Rama] -> [[Variable]]
collectPerchaLevelChildren typeMap [] = []
collectPerchaLevelChildren typeMap branches =
  let children = concatMap perchaChildBranches branches
  in if null children then [] else directPerchaNodes typeMap children : collectPerchaLevelChildren typeMap children

childBranches :: Rama -> [Rama]
childBranches (R _ _ rs) = rs
childBranches (Rc _ _) = []

perchaChildBranches :: Rama -> [Rama]
perchaChildBranches (R _ _ rs) = rs
perchaChildBranches (Rc _ _) = []

directNodes :: [Rama] -> [Variable]
directNodes [] = []
directNodes (R _ p _:rs) = p : directNodes rs
directNodes (Rc _ b:rs) = b : directNodes rs

directPerchaNodes :: TypeMap -> [Rama] -> [Variable]
directPerchaNodes typeMap [] = []
directPerchaNodes typeMap (R _ p _:rs) =
  if isPerchaVar typeMap p then p : directPerchaNodes typeMap rs else directPerchaNodes typeMap rs
directPerchaNodes typeMap (Rc _ _:rs) = directPerchaNodes typeMap rs

filterPerchaNodes :: TypeMap -> [Variable] -> [Variable]
filterPerchaNodes typeMap = filter (\v -> case lookupType v typeMap of Just (Percha _) -> True; _ -> False)

isPerchaVar :: TypeMap -> Variable -> Bool
isPerchaVar typeMap v = case lookupType v typeMap of
  Just (Percha _) -> True
  _ -> False

treeLayout :: [Variable] -> Variable -> [Rama] -> [(Variable, (Double, Double))]
treeLayout _ root ramas =
  let rootPos = (root, (0, 3))
      childPositions = layoutChildren 0 1.5 ramas
  in dedupePositions (rootPos : childPositions)

layoutChildren :: Double -> Double -> [Rama] -> [(Variable, (Double, Double))]
layoutChildren _ _ [] = []
layoutChildren baseX baseY ramas =
  let n = length ramas
      spacing = if n <= 1 then 0 else 2.5
      xs = [baseX + fromIntegral i * spacing - ((fromIntegral (n - 1)) / 2) * spacing | i <- [0 .. n - 1]]
  in concat (zipWith (\branch x -> layoutBranch x baseY branch) ramas xs)

layoutBranch :: Double -> Double -> Rama -> [(Variable, (Double, Double))]
layoutBranch x y (R _ p rs) =
  let current = (p, (x, y))
      descendants = layoutChildren x (y - 1.5) rs
  in current : descendants
layoutBranch x y (Rc _ b) = [(b, (x, y))]

renderEdge :: (Variable, Variable, Maybe Variable) -> String
renderEdge (a, b, Nothing) = "    \\draw (" ++ a ++ ") -- (" ++ b ++ ");"
renderEdge (a, b, Just label) = "    \\draw (" ++ a ++ ") -- node[below left]{" ++ label ++ "} (" ++ b ++ ");"

dedupePositions :: [(Variable, (Double, Double))] -> [(Variable, (Double, Double))]
dedupePositions = foldr addUniquePosition []

addUniquePosition :: (Variable, (Double, Double)) -> [(Variable, (Double, Double))] -> [(Variable, (Double, Double))]
addUniquePosition p [] = [p]
addUniquePosition p@(v, _) xs
  | any (\(x, _) -> x == v) xs = xs
  | otherwise = p : xs

validateDoubleExp :: [String] -> DoubleExp -> Either String ()
validateDoubleExp declNames (Var variable) = validateVariable declNames variable
validateDoubleExp declNames (UMinus e) = validateDoubleExp declNames e
validateDoubleExp declNames (Plus e1 e2) = do
  validateDoubleExp declNames e1
  validateDoubleExp declNames e2
validateDoubleExp declNames (Minus e1 e2) = do
  validateDoubleExp declNames e1
  validateDoubleExp declNames e2
validateDoubleExp declNames (Times e1 e2) = do
  validateDoubleExp declNames e1
  validateDoubleExp declNames e2
validateDoubleExp declNames (Div e1 e2) = do
  validateDoubleExp declNames e1
  validateDoubleExp declNames e2
validateDoubleExp _ _ = Right ()

validateBoolExp :: [String] -> BoolExp -> Either String ()
validateBoolExp declNames (Eq e1 e2) = do
  validateDoubleExp declNames e1
  validateDoubleExp declNames e2
validateBoolExp declNames (Lt e1 e2) = do
  validateDoubleExp declNames e1
  validateDoubleExp declNames e2
validateBoolExp declNames (Gt e1 e2) = do
  validateDoubleExp declNames e1
  validateDoubleExp declNames e2
validateBoolExp declNames (And b1 b2) = do
  validateBoolExp declNames b1
  validateBoolExp declNames b2
validateBoolExp declNames (Or b1 b2) = do
  validateBoolExp declNames b1
  validateBoolExp declNames b2
validateBoolExp declNames (Not b) = validateBoolExp declNames b
validateBoolExp _ _ = Right ()
