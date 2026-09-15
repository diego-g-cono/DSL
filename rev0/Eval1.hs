module Eval1 (eval, evalProgram, renderGraph, validateProgram) where

import AST
import Data.List (nub, intercalate)
import System.IO (writeFile)
import System.IO.Unsafe (unsafePerformIO)

-- Estado para valores numericos del DSL simple.
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

lookfor :: Variable -> State -> Double
lookfor var [] = 0
lookfor var ((x, y):xs) | var == x  = y
                        | otherwise = lookfor var xs

update :: Variable -> Double -> State -> State
update var valor [] = [(var, valor)]
update var valor ((x, y):xs) | var == x  = (var, valor):xs
                             | otherwise = (x, y) : update var valor xs

-- Evalua un programa completo con declaraciones, asignaciones y comando.
evalProgram :: Program -> State
evalProgram program =
  case validateProgram program of
    Left err -> error err
    Right (Program _ asigs comm) ->
      let st0 = foldl evalAsig initState asigs
          typeMap = map (\(Asig var tipo) -> (var, tipo)) asigs
      in evalComm comm st0 typeMap

evalAsig :: State -> Asig -> State
evalAsig st (Asig var tipo) = update var (evalTipo tipo st) st

validateProgram :: Program -> Either String Program
validateProgram p@(Program decls asigs comm) =
  let declNames = map declName decls
      declMap = map (\d -> (declName d, d)) decls
      typeMap = map (\(Asig var tipo) -> (var, tipo)) asigs
      duplicateNames = findDuplicates declNames
  in if not (null duplicateNames)
       then Left ("Hay declaraciones duplicadas: " ++ intercalate ", " duplicateNames)
       else do
         mapM_ (validateVariable declNames) (map asigVar asigs)
         validateComm declNames declMap typeMap comm
         return p

findDuplicates :: [String] -> [String]
findDuplicates [] = []
findDuplicates (x:xs) =
  if x `elem` xs
    then x : findDuplicates (filter (/= x) xs)
    else findDuplicates xs

asigVar :: Asig -> String
asigVar (Asig var _) = var

declName :: Decl -> String
declName (DeclConector x) = x
declName (DeclEslinga x) = x
declName (DeclCarga x) = x

validateVariable :: [String] -> String -> Either String ()
validateVariable declNames var =
  if var `elem` declNames
    then Right ()
    else Left ("La variable '" ++ var ++ "' debe declararse antes de usarse.")

validateComm :: [String] -> [(String, Decl)] -> TypeMap -> Comm -> Either String ()
validateComm declNames declMap _ Skip = Right ()
validateComm declNames declMap _ Draw = Right ()
validateComm declNames declMap _ (Let var exp) = do
  validateVariable declNames var
  validateDoubleExp declNames exp
validateComm declNames declMap typeMap (Seq c1 c2) = do
  validateComm declNames declMap typeMap c1
  validateComm declNames declMap typeMap c2
validateComm declNames declMap typeMap (Cond b c1 c2) = do
  validateBoolExp declNames b
  validateComm declNames declMap typeMap c1
  validateComm declNames declMap typeMap c2
validateComm declNames declMap typeMap (Repeat c b) = do
  validateComm declNames declMap typeMap c
  validateBoolExp declNames b
validateComm declNames declMap typeMap (Connect camino _) = validateCamino declMap typeMap camino

validateCamino :: [(String, Decl)] -> TypeMap -> Camino -> Either String ()
validateCamino declMap typeMap (CaminoBase ns es ms) = do
  validateNodeList declMap ns
  validateNodeConnection declMap typeMap ns es
  validateNodeList declMap ms
  validateNodeConnection declMap typeMap ms es
validateCamino declMap typeMap (CaminoPaso ns es rest) = do
  validateNodeList declMap ns
  validateNodeConnection declMap typeMap ns es
  validateCamino declMap typeMap rest

validateNodeList :: [(String, Decl)] -> NodoList -> Either String ()
validateNodeList _ [] = Right ()
validateNodeList declMap (NodoId x:xs) = do
  case lookup x declMap of
    Just (DeclEslinga _) -> Left ("El identificador '" ++ x ++ "' está declarado como eslinga y no puede usarse como nodo.")
    Just (DeclConector _) -> validateNodeList declMap xs
    Just (DeclCarga _) -> validateNodeList declMap xs
    Nothing -> Left ("El nodo '" ++ x ++ "' no está declarado.")
validateNodeList declMap (NodoNull:xs) = validateNodeList declMap xs

validateNodeConnection :: [(String, Decl)] -> TypeMap -> NodoList -> EslingaList -> Either String ()
validateNodeConnection _ _ _ [] = Right ()
validateNodeConnection declMap typeMap nodes es
  | isPerchaLevel es = do
      mapM_ (validateNodeRole declMap typeMap) nodes
      return ()
  | otherwise = do
      validateEslingaList declMap es
      mapM_ (validateNodeRole declMap typeMap) nodes

validateNodeRole :: [(String, Decl)] -> TypeMap -> Nodo -> Either String ()
validateNodeRole declMap _ (NodoId x) =
  case lookup x declMap of
    Just (DeclConector _) -> Right ()
    Just (DeclCarga _) -> Right ()
    Just (DeclEslinga _) -> Left ("La eslinga '" ++ x ++ "' no puede conectarse como nodo.")
    Nothing -> Left ("El nodo '" ++ x ++ "' no está declarado.")
validateNodeRole _ _ NodoNull = Right ()

validateEslingaList :: [(String, Decl)] -> EslingaList -> Either String ()
validateEslingaList _ [] = Right ()
validateEslingaList declMap (EslingaId x:xs) = do
  case lookup x declMap of
    Just (DeclEslinga _) -> validateEslingaList declMap xs
    Just _ -> Left ("La eslinga '" ++ x ++ "' debe declararse como eslinga.")
    Nothing -> Left ("La eslinga '" ++ x ++ "' no está declarada.")
validateEslingaList declMap (EslingaNull:xs) = validateEslingaList declMap xs

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

evalTipo :: Tipo -> State -> Double
evalTipo (Gancho e) st = evalIntExp e st
evalTipo (Grillete e) st = evalIntExp e st
evalTipo (Percha e) st = evalIntExp e st
evalTipo (Cadena e) st = evalIntExp e st
evalTipo (Sintetica e) st = evalIntExp e st
evalTipo (TipoExpr e) st = evalIntExp e st
evalTipo TipoVacio _ = 0

-- Evaluacion del comando

eval :: Comm -> State
eval p = evalComm p initState []

evalComm :: Comm -> State -> TypeMap -> State
evalComm Skip s _ = s
evalComm Draw s _ = s
evalComm (Let var expInt) s _ = update var (evalIntExp expInt s) s
evalComm (Seq Skip c1) s typeMap = evalComm c1 s typeMap
evalComm (Seq c0 c1) s typeMap = evalComm (Seq Skip c1) (evalComm c0 s typeMap) typeMap
evalComm (Cond b c0 c1) s typeMap = if evalBoolExp b s then evalComm c0 s typeMap else evalComm c1 s typeMap
evalComm (Repeat c b) s typeMap = if evalBoolExp b s then evalComm (Seq c (Repeat c b)) s typeMap else s
evalComm (Connect camino _) s typeMap = unsafePerformIO (do writeFile "izaje.tex" (renderGraphWithTypes camino typeMap)
                                                            return s)

-- Expresiones aritmeticas

evalIntExp :: IntExp -> State -> Double
evalIntExp (Const valor) _ = valor
evalIntExp (Var variable) estado = lookfor variable estado
evalIntExp (UMinus expInt) estado = -(evalIntExp expInt estado)
evalIntExp (Plus exp1 exp2) estado = evalIntExp exp1 estado + evalIntExp exp2 estado
evalIntExp (Minus exp1 exp2) estado = evalIntExp exp1 estado - evalIntExp exp2 estado
evalIntExp (Times exp1 exp2) estado = evalIntExp exp1 estado * evalIntExp exp2 estado
evalIntExp (Div exp1 exp2) estado = evalIntExp exp1 estado / evalIntExp exp2 estado

-- Expresiones booleanas

evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue _ = True
evalBoolExp BFalse _ = False
evalBoolExp (Eq exp1 exp2) estado = evalIntExp exp1 estado == evalIntExp exp2 estado
evalBoolExp (Lt exp1 exp2) estado = evalIntExp exp1 estado < evalIntExp exp2 estado
evalBoolExp (Gt exp1 exp2) estado = evalIntExp exp1 estado > evalIntExp exp2 estado
evalBoolExp (And exp1 exp2) estado = evalBoolExp exp1 estado && evalBoolExp exp2 estado
evalBoolExp (Or exp1 exp2) estado = evalBoolExp exp1 estado || evalBoolExp exp2 estado
evalBoolExp (Not exp1) estado = not (evalBoolExp exp1 estado)

-- Genera un grafo simple en LaTeX con tkz-graph.
renderGraph :: Camino -> String
renderGraph camino = renderGraphWithTypes camino []

renderGraphWithTypes :: Camino -> TypeMap -> String
renderGraphWithTypes camino typeMap =
  let lvls = levels camino
      numLvls = length lvls
      gapY = 2.0
      spacingX = 4.0
      -- flatten nodes preserving order (as names), but ignore null placeholders
      allNodes = nub (map showNodo (filter (not . isNullNodo) (concat lvls)))
      -- map node name to coordinates
      coords = assignCoords lvls numLvls spacingX gapY
      vertexLines = map (renderVertex coords) allNodes
      edgeLines = map renderEdge (edgesInCaminoWithTypes camino typeMap)
  in unlines ( [ "\\begin{tikzpicture}",
                 "\\GraphInit[vstyle=Normal]",
                 "\\SetGraphUnit{4}" ]
               ++ vertexLines
               ++ edgeLines
               ++ [ "\\end{tikzpicture}" ] )

-- Produce list of levels (top-down), each level is a NodoList.
-- Si no hay eslinga entre niveles, se trata como una percha: todos esos nodos
-- quedan en el mismo nivel y se conectan horizontalmente.
levels :: Camino -> [NodoList]
levels (CaminoPaso ns es rest)
  | isPerchaLevel es =
      let r = levels rest
      in case r of
           [] -> [ns]
           (h:hs) -> (ns ++ h) : hs
levels (CaminoPaso ns _ rest) = ns : levels rest
levels (CaminoBase ns es ms)
  | isPerchaLevel es = [ns ++ ms]
levels (CaminoBase ns _ ms) = [ns, ms]

isPerchaLevel :: EslingaList -> Bool
isPerchaLevel es = null es || all (== EslingaNull) es

-- Assign coordinates to each node name based on its level and position
assignCoords :: [NodoList] -> Int -> Double -> Double -> [(String, (Double, Double))]
assignCoords lvls numLvls spacingX gapY = concat $ zipWith assignLevel [0..] lvls
  where
    assignLevel :: Int -> NodoList -> [(String, (Double, Double))]
    assignLevel idx nodes =
      let k = length nodes
          y = (fromIntegral (numLvls - 1 - idx)) * gapY
          startX = - (fromIntegral (k - 1) / 2.0) * spacingX
          xs = [ startX + fromIntegral j * spacingX | j <- [0..(k-1)] ]
          visiblePairs = [ (showNodo n, (x, y)) | (n, x) <- zip nodes xs, not (isNullNodo n) ]
      in visiblePairs

renderVertex :: [(String, (Double, Double))] -> String -> String
renderVertex coords name =
  case lookup name coords of
    Just (x, y) -> "\\Vertex[x=" ++ formatCoord x ++ ",y=" ++ formatCoord y ++ "]{" ++ name ++ "}"
    Nothing -> "\\Vertex{" ++ name ++ "}"

formatCoord :: Double -> String
formatCoord v = if abs (v - fromInteger (round v)) < 1e-6
                then show (round v)
                else show v

nodesInCamino :: Camino -> [String]
nodesInCamino (CaminoBase ns _ ms) = map showNodo (ns ++ ms)
nodesInCamino (CaminoPaso ns _ rest) = map showNodo ns ++ nodesInCamino rest

edgesInCamino :: Camino -> [(String, String, String)]
edgesInCamino camino = edgesInCaminoWithTypes camino []

edgesInCaminoWithTypes :: Camino -> TypeMap -> [(String, String, String)]
edgesInCaminoWithTypes (CaminoPaso ns es rest) typeMap =
  let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
      lowerEdges = if isPerchaLevel es then [] else linkNodes ns (firstNodes rest) es
  in perchaEdges ++ lowerEdges ++ edgesInCaminoWithTypes rest typeMap

edgesInCaminoWithTypes (CaminoBase ns es ms) typeMap =
  let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
      lowerEdges = if isPerchaLevel es then [] else linkNodes ns ms es
  in perchaEdges ++ lowerEdges

firstNodes :: Camino -> NodoList
firstNodes (CaminoPaso ns _ _) = ns
firstNodes (CaminoBase ns _ _) = ns

linkNodes :: NodoList -> NodoList -> EslingaList -> [(String, String, String)]
linkNodes [] _ _ = []
linkNodes _ [] _ = []
linkNodes prev next es =
  let prevNames = map showNodo (filter (not . isNullNodo) prev)
      nextNames = map showNodo (filter (not . isNullNodo) next)
      realEs = filter (/= EslingaNull) es
  in if null prevNames || null nextNames || null realEs
       then []
       else map (\(idx, e) ->
                    let aIdx = nodeIndex idx (length prevNames) (length realEs)
                        bIdx = nodeIndex idx (length nextNames) (length realEs)
                        a = prevNames !! aIdx
                        b = nextNames !! bIdx
                    in (a, b, showEslinga e)) (zip [0 ..] realEs)

nodeIndex :: Int -> Int -> Int -> Int
nodeIndex idx total len
  | total <= 1 = 0
  | len <= 1 = 0
  | otherwise = round (fromIntegral idx * fromIntegral (total - 1) / fromIntegral (len - 1))

horizontalPercha :: NodoList -> [(String, String, String)]
horizontalPercha xs =
  let names = map showNodo (filter (not . isNullNodo) xs)
  in case names of
       [] -> []
       [_] -> []
       _ -> zipWith (\a b -> (a, b, "")) (init names) (drop 1 names)

arePerchaNodes :: NodoList -> TypeMap -> Bool
arePerchaNodes xs typeMap =
  let names = [x | NodoId x <- filter (not . isNullNodo) xs]
  in not (null names) && all (\x -> isPerchaType x typeMap) names

horizontalPerchaByType :: NodoList -> TypeMap -> [(String, String, String)]
horizontalPerchaByType xs typeMap =
  if arePerchaNodes xs typeMap
    then horizontalPercha xs
    else []

renderEdge :: (String, String, String) -> String
renderEdge (a, b, "") = "\\Edge({" ++ a ++ "})({" ++ b ++ "})"
renderEdge (a, b, label) = "\\Edge[label={" ++ label ++ "}]({" ++ a ++ "})({" ++ b ++ "})"

isNullNodo :: Nodo -> Bool
isNullNodo NodoNull = True
isNullNodo _ = False

showNodo :: Nodo -> String
showNodo (NodoId x) = x
showNodo NodoNull = "null"

showEslinga :: Eslinga -> String
showEslinga (EslingaId x) = x
showEslinga EslingaNull = "null"
