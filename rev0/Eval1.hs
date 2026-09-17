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
          (finalState, finalTypeMap) = evalCommWithTypeMap comm st0 typeMap
      in finalState

evalAsig :: State -> Asig -> State
evalAsig st (Asig var tipo) = update var (evalTipo tipo st) st

validateProgram :: Program -> Either String Program
validateProgram p@(Program decls asigs comm) =
  let declNames = map declName decls
      declMap = map (\d -> (declName d, d)) decls
      state = foldl evalAsig initState asigs
      typeMap = map (\(Asig var tipo) -> (var, resolveTipo state tipo)) asigs
      duplicateNames = findDuplicates declNames
  in if not (null duplicateNames)
       then Left ("Hay declaraciones duplicadas: " ++ intercalate ", " duplicateNames)
       else do
         mapM_ (validateAsigType declMap) asigs
         mapM_ (validateVariable declNames) (map asigVar asigs)
         _ <- validateCommWithTypeMap declNames declMap typeMap comm
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
  mapM_ (validateVariable declNames) vars
  mapM_ (validateAsigType declMap) (map (\var -> Asig var tipo) vars)
  Right (foldl (\acc var -> updateTypeMap var (resolveTipo (foldl evalAsig initState (map (\v -> Asig v tipo) vars)) tipo) acc) typeMap vars)
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
validateCommWithTypeMap declNames declMap typeMap (Connect camino _) = do
  validateCamino declMap typeMap camino
  Right typeMap

updateTypeMap :: Variable -> Tipo -> TypeMap -> TypeMap
updateTypeMap var tipo [] = [(var, tipo)]
updateTypeMap var tipo ((x, y):xs)
  | var == x  = (var, tipo):xs
  | otherwise = (x, y) : updateTypeMap var tipo xs

validateCamino :: [(String, Decl)] -> TypeMap -> Camino -> Either String ()
validateCamino declMap typeMap (CaminoBase ns es ms) = do
  validateNodeList declMap ns
  validateNodeConnection declMap typeMap ns es
  validateNodeList declMap ms
  validateCargaList declMap ms
  validateLevelCapacity declMap typeMap ns es ms
  validateNodeConnection declMap typeMap ms es
validateCamino declMap typeMap (CaminoPaso ns es rest) = do
  validateNodeList declMap ns
  validateNodeConnection declMap typeMap ns es
  validateLevelCapacity declMap typeMap ns es (firstNodes rest)
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

validateCargaList :: [(String, Decl)] -> NodoList -> Either String ()
validateCargaList _ [] = Right ()
validateCargaList declMap (NodoNull:xs) = validateCargaList declMap xs
validateCargaList declMap (NodoId x:xs) =
  case lookup x declMap of
    Just (DeclCarga _) -> validateCargaList declMap xs
    Just _ -> Left ("El nodo final '" ++ x ++ "' debe declararse como carga.")
    Nothing -> Left ("El nodo final '" ++ x ++ "' no está declarado.")

validateLevelCapacity :: [(String, Decl)] -> TypeMap -> NodoList -> EslingaList -> NodoList -> Either String ()
validateLevelCapacity _ typeMap prev es next = do
  let prevNames = map showNodo (filter (not . isNullNodo) prev)
      nextNames = map showNodo (filter (not . isNullNodo) next)
      realEs = filter (/= EslingaNull) es
      pairs = buildLevelPairs prevNames nextNames realEs
  mapM_ (\(src, dst, e) -> do
          srcVal <- lookupAssignedValue src typeMap
          dstVal <- lookupAssignedValue dst typeMap
          eVal <- lookupAssignedValue (showEslinga e) typeMap
          if eVal >= dstVal
            then Right ()
            else Left ("La eslinga '" ++ showEslinga e ++ "' debe tener un valor mayor o igual a la carga '" ++ dst ++ "'.")) pairs
  mapM_ (\(node, total) -> do
          nodeVal <- lookupAssignedValue node typeMap
          if nodeVal >= total
            then Right ()
            else Left ("El nodo '" ++ node ++ "' debe soportar al menos " ++ show total ++ " y tiene valor " ++ show nodeVal ++ ".")) (aggregateBySource pairs typeMap)

buildLevelPairs :: [String] -> [String] -> [Eslinga] -> [(String, String, Eslinga)]
buildLevelPairs prevNames nextNames realEs =
  map (\(idx, e) ->
         let srcIdx = nodeIndex idx (length prevNames) (length realEs)
             dstIdx = nodeIndex idx (length nextNames) (length realEs)
             src = prevNames !! srcIdx
             dst = nextNames !! dstIdx
         in (src, dst, e)) (zip [0 ..] realEs)

aggregateBySource :: [(String, String, Eslinga)] -> TypeMap -> [(String, Double)]
aggregateBySource pairs typeMap =
  foldl (\acc (src, _, e) ->
           let val = lookupAssignedValueOrZero (showEslinga e) typeMap
           in case lookup src acc of
                Just total -> map (\(n, t) -> if n == src then (n, t + val) else (n, t)) acc
                Nothing -> acc ++ [(src, val)]) [] pairs

lookupAssignedValueOrZero :: String -> TypeMap -> Double
lookupAssignedValueOrZero name typeMap =
  case lookup name typeMap of
    Just t -> tipoValor t
    Nothing -> 0

lookupAssignedValue :: String -> TypeMap -> Either String Double
lookupAssignedValue name typeMap =
  case lookup name typeMap of
    Just t -> Right (tipoValor t)
    Nothing -> Left ("La variable '" ++ name ++ "' no está asignada.")

tipoValor :: Tipo -> Double
tipoValor (Gancho e) = evalTipoValue e
tipoValor (Grillete e) = evalTipoValue e
tipoValor (Percha e) = evalTipoValue e
tipoValor (Cadena e) = evalTipoValue e
tipoValor (Sintetica e) = evalTipoValue e
tipoValor (TipoExpr e) = evalTipoValue e
tipoValor TipoVacio = 0

evalTipoValue :: DoubleExp -> Double
evalTipoValue (Const valor) = valor
evalTipoValue (Var variable) = 0
evalTipoValue (UMinus e) = - evalTipoValue e
evalTipoValue (Plus e1 e2) = evalTipoValue e1 + evalTipoValue e2
evalTipoValue (Minus e1 e2) = evalTipoValue e1 - evalTipoValue e2
evalTipoValue (Times e1 e2) = evalTipoValue e1 * evalTipoValue e2
evalTipoValue (Div e1 e2) = evalTipoValue e1 / evalTipoValue e2

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
evalComm comm s typeMap = fst (evalCommWithTypeMap comm s typeMap)

evalCommWithTypeMap :: Comm -> State -> TypeMap -> (State, TypeMap)
evalCommWithTypeMap Skip s typeMap = (s, typeMap)
evalCommWithTypeMap Draw s typeMap = (s, typeMap)
evalCommWithTypeMap (Let var expInt) s typeMap = (update var (evalIntExp expInt s) s, typeMap)
evalCommWithTypeMap (Each vars tipo) s typeMap =
  let newState = foldl (\acc var -> update var (evalTipo tipo acc) acc) s vars
      newTypeMap = foldl (\acc var -> updateTypeMap var (resolveTipo s tipo) acc) typeMap vars
  in (newState, newTypeMap)
evalCommWithTypeMap (Seq Skip c1) s typeMap = evalCommWithTypeMap c1 s typeMap
evalCommWithTypeMap (Seq c0 c1) s typeMap =
  let (s1, typeMap1) = evalCommWithTypeMap c0 s typeMap
  in evalCommWithTypeMap c1 s1 typeMap1
evalCommWithTypeMap (Cond b c0 c1) s typeMap =
  if evalBoolExp b s then evalCommWithTypeMap c0 s typeMap else evalCommWithTypeMap c1 s typeMap
evalCommWithTypeMap (Repeat c b) s typeMap =
  if evalBoolExp b s then evalCommWithTypeMap (Seq c (Repeat c b)) s typeMap else (s, typeMap)
evalCommWithTypeMap (Connect camino _) s typeMap =
  let resolvedTypeMap = map (\(var, tipo) -> (var, resolveTipo s tipo)) typeMap
  in unsafePerformIO (writeFile "izaje.tex" (renderGraphWithTypes camino resolvedTypeMap) >> return (s, typeMap))

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
      vertexLines = concatMap (renderVertex coords typeMap) allNodes
      edgeLines = map (renderEdge coords) (edgesInCaminoWithTypes camino typeMap)
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

renderVertex :: [(String, (Double, Double))] -> TypeMap -> String -> [String]
renderVertex coords typeMap name =
  case lookup name coords of
    Just (x, y) ->
      let posOpts = "x=" ++ formatCoord x ++ ",y=" ++ formatCoord y
          vertexLine = "\\Vertex[" ++ posOpts ++ "]{" ++ name ++ "}"
          labelLines = case lookup name typeMap of
            Just t ->
              let cap = formatCapacity t ++ "tn"
                  -- choose anchor according to x coordinate
                  anchorLine | x < -1e-6 = "\\node[anchor=east]  at (" ++ name ++ ".west)  {" ++ cap ++ "};"
                             | x > 1e-6  = "\\node[anchor=west]  at (" ++ name ++ ".east)  {" ++ cap ++ "};"
                             | otherwise = "\\node[anchor=south] at (" ++ name ++ ".north) {" ++ cap ++ "};"
              in [anchorLine]
            Nothing -> []
      in vertexLine : labelLines
    Nothing -> ["\\Vertex{" ++ name ++ "}"]

formatCapacity :: Tipo -> String
formatCapacity t = formatCoord (tipoValor t)

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
      lowerEdges = if isPerchaLevel es then [] else linkNodes ns (firstNodes rest) es typeMap
  in perchaEdges ++ lowerEdges ++ edgesInCaminoWithTypes rest typeMap

edgesInCaminoWithTypes (CaminoBase ns es ms) typeMap =
  let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
      lowerEdges = if isPerchaLevel es then [] else linkNodes ns ms es typeMap
  in perchaEdges ++ lowerEdges

firstNodes :: Camino -> NodoList
firstNodes (CaminoPaso ns _ _) = ns
firstNodes (CaminoBase ns _ _) = ns

linkNodes :: NodoList -> NodoList -> EslingaList -> TypeMap -> [(String, String, String)]
linkNodes [] _ _ _ = []
linkNodes _ [] _ _ = []
linkNodes prev next es typeMap =
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
              ename = showEslinga e
              label = eslingaLabel ename typeMap
          in (a, b, label)) (zip [0 ..] realEs)

eslingaLabel :: String -> TypeMap -> String
eslingaLabel "null" _ = ""
eslingaLabel name typeMap =
  case lookup name typeMap of
  Just t -> name ++ " " ++ formatCapacity t ++ "tn"
  Nothing -> name

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

renderEdge :: [(String, (Double, Double))] -> (String, String, String) -> String
renderEdge coords (a, b, lbl) =
  let ma = lookup a coords
      mb = lookup b coords
      pos = case (ma, mb) of
        (Just (ax, ay), Just (bx, by)) ->
          let dx = bx - ax
              dy = by - ay
          in if abs dx < 1e-6
               then if ay < by then "right" else "left"
               else if abs dy < 1e-6
                      then "below"
                      else if dx > 0 then "below right" else "below left"
        _ -> "midway"
  in if null lbl
       then "\\draw (" ++ a ++ ") -- (" ++ b ++ ");"
       else "\\draw (" ++ a ++ ") -- node[" ++ pos ++ "]{" ++ lbl ++ "} (" ++ b ++ ");"

isNullNodo :: Nodo -> Bool
isNullNodo NodoNull = True
isNullNodo _ = False

showNodo :: Nodo -> String
showNodo (NodoId x) = x
showNodo NodoNull = "null"

showEslinga :: Eslinga -> String
showEslinga (EslingaId x) = x
showEslinga EslingaNull = "null"
