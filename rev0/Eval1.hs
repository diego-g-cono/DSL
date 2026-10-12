module Eval1 (eval, evalProgram, renderGraph, validateProgram) where

import AST
import Data.List (nub, intercalate)
import System.IO (writeFile)
import System.IO.Unsafe (unsafePerformIO)

-- Estado para valores numericos
type State = [(Variable, Double)]
type TypeMap = [(Variable, Tipo)]

-- | Inicializa el estado de ejecución con una tabla vacía de variables.
initState :: State
initState = []

-- | Busca el tipo asociado a una variable en el mapa de tipos del programa.
lookupType :: Variable -> TypeMap -> Maybe Tipo
lookupType = lookup

-- | Determina si una variable corresponde a un conector de tipo percha.
isPerchaType :: Variable -> TypeMap -> Bool
isPerchaType var typeMap = case lookupType var typeMap 
                             of Just (Percha _) -> True
                                _ -> False

-- | Determina si una variable corresponde a un conector de tipo gancho o grillete.
isGanchoOrGrilleteType :: Variable -> TypeMap -> Bool
isGanchoOrGrilleteType var typeMap = case lookupType var typeMap
                                      of Just (Gancho _) -> True
                                         Just (Grillete _) -> True
                                         _ -> False

-- | Recupera el valor almacenado de una variable en el estado actual.
lookfor :: Variable -> State -> Double
lookfor var [] = 0
lookfor var ((x, y):xs) | var == x  = y
                        | otherwise = lookfor var xs

-- | Actualiza o agrega una variable con su nuevo valor en el estado.
update :: Variable -> Double -> State -> State
update var valor [] = [(var, valor)]
update var valor ((x, y):xs) | var == x  = (var, valor):xs
                             | otherwise = (x, y) : update var valor xs

-- | Ejecuta un programa completo validando primero la estructura y luego evaluando
--   sus asignaciones y el comando principal.
evalProgram :: Program -> State
evalProgram program = case validateProgram program 
                      of Left err -> error err
                         Right (Program _ asigs comm) -> let st0 = evalAsigs asigs initState
                                                             typeMap = makeTypeMap asigs
                                                             (finalState, _) = evalCommWithTypeMap comm st0 typeMap
                                                          in finalState
    

-- | Aplica secuencialmente una lista de asignaciones sobre un estado inicial.
evalAsigs :: [Asig] -> State -> State
evalAsigs [] st = st
evalAsigs (a:as) st = evalAsigs as (evalAsig st a)

-- | Construye un mapa de tipos a partir de las asignaciones declaradas.
makeTypeMap :: [Asig] -> [(Variable, Tipo)]
makeTypeMap [] = []
makeTypeMap (Asig var tipo : as) = (var, tipo) : makeTypeMap as

-- | Evalúa una asignación individual y devuelve el estado actualizado.
evalAsig :: State -> Asig -> State
evalAsig st (Asig var tipo) = update var (evalTipo tipo st) st

-- | Valida que el programa sea consistente: declaraciones únicas, tipos válidos y
--   referencias y conexiones correctas dentro del comando.
validateProgram :: Program -> Either String Program
validateProgram p@(Program decls asigs comm) = let declNames = map declName decls
                                                   declMap = map (\d -> (declName d, d)) decls
                                                   state = evalAsigs asigs initState
                                                   typeMap = asigTypeMap state asigs
                                                   duplicateNames = findDuplicates declNames
                                                in if not (null duplicateNames)
                                                   then Left ("Hay declaraciones duplicadas: " ++ intercalate ", " duplicateNames)
                                                   else do validateAsigs declMap asigs
                                                           validateAsigVars declNames asigs
                                                           _ <- validateCommWithTypeMap declNames declMap typeMap comm
                                                           return p

-- | Resuelve cada tipo declarando su valor numérico con el estado disponible.
asigTypeMap :: State -> [Asig] -> TypeMap
asigTypeMap _ [] = []
asigTypeMap state (Asig var tipo:as) = (var, resolveTipo state tipo) : asigTypeMap state as

-- | Revisa que todas las asignaciones tengan tipos compatibles con sus declaraciones.
validateAsigs :: [(String, Decl)] -> [Asig] -> Either String ()
validateAsigs _ [] = Right ()
validateAsigs declMap (a:as) = do validateAsigType declMap a
                                  validateAsigs declMap as

-- | Verifica que cada variable usada en una asignación haya sido declarada antes.
validateAsigVars :: [String] -> [Asig] -> Either String ()
validateAsigVars _ [] = Right ()
validateAsigVars declNames (Asig var _:as) = do validateVariable declNames var
                                                validateAsigVars declNames as

-- | Detecta nombres duplicados en una lista de declaraciones para prevenir conflictos.
findDuplicates :: [String] -> [String]
findDuplicates [] = []
findDuplicates (x:xs) = if x `elem` xs
                        then x : findDuplicates (filter (/= x) xs)
                        else findDuplicates xs

-- | Extrae el nombre de una variable asignada.
asigVar :: Asig -> String
asigVar (Asig var _) = var

-- | Obtiene el nombre de una declaración independientemente de su tipo.
declName :: Decl -> String
declName (DeclConector x) = x
declName (DeclEslinga x) = x
declName (DeclCarga x) = x

-- | Comprueba si una variable existe dentro del conjunto de declaraciones válidas.
validateVariable :: [String] -> String -> Either String ()
validateVariable declNames var =
  if var `elem` declNames
    then Right ()
    else Left ("La variable '" ++ var ++ "' debe declararse antes de usarse.")

-- | Sustituye variables por sus valores actuales dentro de un tipo para evaluar su capacidad.
resolveTipo :: State -> Tipo -> Tipo
resolveTipo st (Gancho e) = Gancho (resolveDoubleExp st e)
resolveTipo st (Grillete e) = Grillete (resolveDoubleExp st e)
resolveTipo st (Percha e) = Percha (resolveDoubleExp st e)
resolveTipo st (Cadena e) = Cadena (resolveDoubleExp st e)
resolveTipo st (Sintetica e) = Sintetica (resolveDoubleExp st e)
resolveTipo st (TipoExpr e) = TipoExpr (resolveDoubleExp st e)
resolveTipo _ TipoVacio = TipoVacio

-- | Evalúa una expresión numérica reemplazando variables por constantes conocidas del estado.
resolveDoubleExp :: State -> DoubleExp -> DoubleExp
resolveDoubleExp _ (Const valor) = Const valor
resolveDoubleExp st (Var variable) = Const (lookfor variable st)
resolveDoubleExp st (UMinus e) = UMinus (resolveDoubleExp st e)
resolveDoubleExp st (Plus e1 e2) = Plus (resolveDoubleExp st e1) (resolveDoubleExp st e2)
resolveDoubleExp st (Minus e1 e2) = Minus (resolveDoubleExp st e1) (resolveDoubleExp st e2)
resolveDoubleExp st (Times e1 e2) = Times (resolveDoubleExp st e1) (resolveDoubleExp st e2)
resolveDoubleExp st (Div e1 e2) = Div (resolveDoubleExp st e1) (resolveDoubleExp st e2)

-- | Verifica que el tipo asignado a una variable sea compatible con el tipo de declaración.
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

-- | Revisa un comando y devuelve el mapa actualizado de tipos si la estructura es válida.
validateComm :: [String] -> [(String, Decl)] -> TypeMap -> Comm -> Either String ()
validateComm declNames declMap typeMap comm = do _ <- validateCommWithTypeMap declNames declMap typeMap comm
                                                 return ()

-- | Validación recursiva del comando, incluyendo secuencias, condicionales, repeticiones y conexiones.
validateCommWithTypeMap :: [String] -> [(String, Decl)] -> TypeMap -> Comm -> Either String TypeMap
validateCommWithTypeMap declNames declMap typeMap Skip = Right typeMap
validateCommWithTypeMap declNames declMap typeMap Draw = Right typeMap
validateCommWithTypeMap declNames declMap typeMap (Let var exp) = do validateVariable declNames var
                                                                     validateDoubleExp declNames exp
                                                                     Right typeMap
validateCommWithTypeMap declNames declMap typeMap (Each vars tipo) = do validateEachVariables declNames vars
                                                                        validateEachAsigns declMap vars tipo
                                                                        let eachState = evalAsigs (map (\var -> Asig var tipo) vars) initState
                                                                            resolvedTipo = resolveTipo eachState tipo
                                                                        Right (updateEachTypeInValidation vars resolvedTipo typeMap)
validateCommWithTypeMap declNames declMap typeMap (Seq c1 c2) = do typeMap1 <- validateCommWithTypeMap declNames declMap typeMap c1
                                                                   validateCommWithTypeMap declNames declMap typeMap1 c2
validateCommWithTypeMap declNames declMap typeMap (Cond b c1 c2) = do validateBoolExp declNames b
                                                                      validateComm declNames declMap typeMap c1
                                                                      validateComm declNames declMap typeMap c2
                                                                      Right typeMap
validateCommWithTypeMap declNames declMap typeMap (Repeat c b) = do validateComm declNames declMap typeMap c
                                                                    validateBoolExp declNames b
                                                                    Right typeMap
validateCommWithTypeMap declNames declMap typeMap (Connect camino _) = do validateCamino declMap typeMap camino
                                                                          Right typeMap

-- | Asegura que todas las variables de un bloque `each` estén declaradas.
validateEachVariables :: [String] -> [Variable] -> Either String ()
validateEachVariables _ [] = Right ()
validateEachVariables declNames (var:vars) = do
  validateVariable declNames var
  validateEachVariables declNames vars

-- | Verifica que cada variable del `each` pueda recibir el tipo indicado.
validateEachAsigns :: [(String, Decl)] -> [Variable] -> Tipo -> Either String ()
validateEachAsigns _ [] _ = Right ()
validateEachAsigns declMap (var:vars) tipo = do
  validateAsigType declMap (Asig var tipo)
  validateEachAsigns declMap vars tipo

-- | Actualiza el mapa de tipos con las variables generadas por un `each` durante la validación.
updateEachTypeInValidation :: [Variable] -> Tipo -> TypeMap -> TypeMap
updateEachTypeInValidation [] _ typeMap = typeMap
updateEachTypeInValidation (var:vars) tipo typeMap =
  updateEachTypeInValidation vars tipo (updateTypeMap var tipo typeMap)

-- | Inserta o reemplaza un tipo para una variable en el mapa de tipos.
updateTypeMap :: Variable -> Tipo -> TypeMap -> TypeMap
updateTypeMap var tipo [] = [(var, tipo)]
updateTypeMap var tipo ((x, y):xs)
  | var == x  = (var, tipo):xs
  | otherwise = (x, y) : updateTypeMap var tipo xs

-- | Valida la integridad de un camino, incluyendo nodos, eslingas y capacidad de cada nivel.
validateCamino :: [(String, Decl)] -> TypeMap -> Camino -> Either String ()
validateCamino declMap typeMap (CaminoBase ns es ms) = do
  validateNodeList declMap ns
  validateNodeConnection declMap typeMap ns es
  validateNodeList declMap ms
  validateCargaList declMap typeMap ms
  validateLevelCapacity declMap typeMap ns es ms
  validateNodeConnection declMap typeMap ms es
validateCamino declMap typeMap (CaminoPaso ns es rest) = do
  validateNodeList declMap ns
  validateNodeConnection declMap typeMap ns es
  validateLevelCapacity declMap typeMap ns es (firstNodes rest)
  validateCamino declMap typeMap rest
validateCamino declMap typeMap (CaminoFinal (NodoEslinga ns es) cargas) = do
  validateNodeList declMap ns
  validateFinalConnectorNodes declMap typeMap ns
  validateNodeConnection declMap typeMap ns es
  validateFinalCargaList declMap typeMap cargas
  validateLevelCapacity declMap typeMap ns es (cargaToNodoList cargas)
  validateNodeConnection declMap typeMap (cargaToNodoList cargas) es
validateCamino declMap typeMap (CaminoFinal (EslingaNodo es ns) cargas) = do
  validateNodeList declMap ns
  validateFinalConnectorNodes declMap typeMap ns
  validateNodeConnection declMap typeMap ns es
  validateFinalCargaList declMap typeMap cargas
  validateLevelCapacity declMap typeMap ns es (cargaToNodoList cargas)
  validateNodeConnection declMap typeMap (cargaToNodoList cargas) es

-- | Asegura que en el nivel final sólo se aceptan conectores de tipo gancho o grillete.
validateFinalConnectorNodes :: [(String, Decl)] -> TypeMap -> NodoList -> Either String ()
validateFinalConnectorNodes _ _ [] = Right ()
validateFinalConnectorNodes declMap typeMap (NodoNull:xs) = validateFinalConnectorNodes declMap typeMap xs
validateFinalConnectorNodes declMap typeMap (NodoId x:xs) =
  case lookup x declMap of
    Just (DeclConector _) ->
      if isGanchoOrGrilleteType x typeMap
        then validateFinalConnectorNodes declMap typeMap xs
        else Left ("El conector '" ++ x ++ "' debe ser de tipo gancho o grillete antes de la carga final.")
    Just (DeclCarga _) -> validateFinalConnectorNodes declMap typeMap xs
    Just _ -> Left ("El nodo '" ++ x ++ "' no es válido en la conexión final.")
    Nothing -> Left ("El nodo '" ++ x ++ "' no está declarado.")

-- | Convierte una lista de cargas a nodos para reutilizar la validación del nivel final.
cargaToNodoList :: CargaList -> NodoList
cargaToNodoList [] = []
cargaToNodoList (CargaNull:xs) = NodoNull : cargaToNodoList xs
cargaToNodoList (CargaId x:xs) = NodoId x : cargaToNodoList xs

-- | Valida una lista final de cargas, aceptando sólo cargas declaradas.
validateFinalCargaList :: [(String, Decl)] -> TypeMap -> CargaList -> Either String ()
validateFinalCargaList _ _ [] = Right ()
validateFinalCargaList declMap typeMap (CargaNull:xs) = validateFinalCargaList declMap typeMap xs
validateFinalCargaList declMap typeMap (CargaId x:xs) =
  case lookup x declMap of
    Just (DeclCarga _) -> validateFinalCargaList declMap typeMap xs
    Just (DeclConector _) ->
      if isGanchoOrGrilleteType x typeMap
        then validateFinalCargaList declMap typeMap xs
        else Left ("El nodo final '" ++ x ++ "' debe declararse como carga.")
    Just _ -> Left ("El nodo final '" ++ x ++ "' debe declararse como carga.")
    Nothing -> Left ("El nodo final '" ++ x ++ "' no está declarado.")

-- | Comprueba que la lista de nodos tenga sólo identificadores declarados y no eslingas.
validateNodeList :: [(String, Decl)] -> NodoList -> Either String ()
validateNodeList _ [] = Right ()
validateNodeList declMap (NodoId x:xs) = do
  case lookup x declMap of
    Just (DeclEslinga _) -> Left ("El identificador '" ++ x ++ "' está declarado como eslinga y no puede usarse como nodo.")
    Just (DeclConector _) -> validateNodeList declMap xs
    Just (DeclCarga _) -> validateNodeList declMap xs
    Nothing -> Left ("El nodo '" ++ x ++ "' no está declarado.")
validateNodeList declMap (NodoNull:xs) = validateNodeList declMap xs

-- | Comprueba que los nodos principales y las eslingas conectadas sean consistentes con sus roles.
validateNodeConnection :: [(String, Decl)] -> TypeMap -> NodoList -> EslingaList -> Either String ()
validateNodeConnection _ _ _ [] = Right ()
validateNodeConnection declMap typeMap nodes es
  | isPerchaLevel es = validateNodeRoles declMap typeMap nodes
  | otherwise = do
      validateEslingaList declMap es
      validateNodeRoles declMap typeMap nodes

-- | Revisa cada nodo de una lista para garantizar que tenga una declaración válida.
validateNodeRoles :: [(String, Decl)] -> TypeMap -> NodoList -> Either String ()
validateNodeRoles _ _ [] = Right ()
validateNodeRoles declMap typeMap (node:nodes) = do
  validateNodeRole declMap typeMap node
  validateNodeRoles declMap typeMap nodes

-- | Verifica si un nodo concreto puede participar como punto de conexión del camino.
validateNodeRole :: [(String, Decl)] -> TypeMap -> Nodo -> Either String ()
validateNodeRole declMap _ (NodoId x) =
  case lookup x declMap of
    Just (DeclConector _) -> Right ()
    Just (DeclCarga _) -> Right ()
    Just (DeclEslinga _) -> Left ("La eslinga '" ++ x ++ "' no puede conectarse como nodo.")
    Nothing -> Left ("El nodo '" ++ x ++ "' no está declarado.")
validateNodeRole _ _ NodoNull = Right ()

-- | Asegura que el último nivel del camino esté compuesto por cargas válidas,
--   permitiendo además conectores tipo gancho o grillete como punto de apoyo.
validateCargaList :: [(String, Decl)] -> TypeMap -> NodoList -> Either String ()
validateCargaList _ _ [] = Right ()
validateCargaList declMap typeMap (NodoNull:xs) = validateCargaList declMap typeMap xs
validateCargaList declMap typeMap (NodoId x:xs) =
  case lookup x declMap of
    Just (DeclCarga _) -> validateCargaList declMap typeMap xs
    Just (DeclConector _) ->
      if isGanchoOrGrilleteType x typeMap
        then validateCargaList declMap typeMap xs
        else Left ("El nodo final '" ++ x ++ "' debe declararse como carga o como conector gancho/grillete.")
    Just _ -> Left ("El nodo final '" ++ x ++ "' debe declararse como carga o como conector gancho/grillete.")
    Nothing -> Left ("El nodo final '" ++ x ++ "' no está declarado.")

-- | Comprueba que cada nivel del camino no supere la capacidad soportada por sus fuentes y eslingas.
validateLevelCapacity :: [(String, Decl)] -> TypeMap -> NodoList -> EslingaList -> NodoList -> Either String ()
validateLevelCapacity _ typeMap prev es next = do
  let prevNames = map showNodo (filter (not . isNullNodo) prev)
      nextNames = map showNodo (filter (not . isNullNodo) next)
      realEs = filter (/= EslingaNull) es
      pairs = buildLevelPairs prevNames nextNames realEs
  validateLevelPairs pairs typeMap
  validateSourceTotals (aggregateBySource pairs typeMap) typeMap

-- | Revisa cada conexión de nivel para confirmar que la eslinga no está por debajo de la carga.
validateLevelPairs :: [(String, String, Eslinga)] -> TypeMap -> Either String ()
validateLevelPairs [] _ = Right ()
validateLevelPairs ((src, dst, e):pairs) typeMap = do
  srcVal <- lookupAssignedValue src typeMap
  dstVal <- lookupAssignedValue dst typeMap
  eVal <- lookupAssignedValue (showEslinga e) typeMap
  if eVal >= dstVal
    then validateLevelPairs pairs typeMap
    else Left ("La eslinga '" ++ showEslinga e ++ "' debe tener un valor mayor o igual a la carga '" ++ dst ++ "'.")

-- | Verifica que cada fuente del camino pueda sostener la suma total de las cargas que recibe.
validateSourceTotals :: [(String, Double)] -> TypeMap -> Either String ()
validateSourceTotals [] _ = Right ()
validateSourceTotals ((node, total):rest) typeMap = do
  nodeVal <- lookupAssignedValue node typeMap
  if nodeVal >= total
    then validateSourceTotals rest typeMap
    else Left ("El nodo '" ++ node ++ "' debe soportar al menos " ++ show total ++ " y tiene valor " ++ show nodeVal ++ ".")

-- | Genera las parejas origen-destino asociadas a cada eslinga del nivel.
buildLevelPairs :: [String] -> [String] -> [Eslinga] -> [(String, String, Eslinga)]
buildLevelPairs prevNames nextNames realEs =
  map (\(idx, e) ->
         let srcIdx = nodeIndex idx (length prevNames) (length realEs)
             dstIdx = nodeIndex idx (length nextNames) (length realEs)
             src = prevNames !! srcIdx
             dst = nextNames !! dstIdx
         in (src, dst, e)) (zip [0 ..] realEs)

-- | Suma la carga total asociada a cada nodo origen para comprobar la resistencia del circuito.
aggregateBySource :: [(String, String, Eslinga)] -> TypeMap -> [(String, Double)]
aggregateBySource [] _ = []
aggregateBySource ((src, _, e):rest) typeMap =
  let val = lookupAssignedValueOrZero (showEslinga e) typeMap
      acc = aggregateBySource rest typeMap
  in addSourceTotal src val acc

-- | Acumula en una lista el peso total que aporta cada origen a sus conexiones.
addSourceTotal :: String -> Double -> [(String, Double)] -> [(String, Double)]
addSourceTotal src val [] = [(src, val)]
addSourceTotal src val ((name, total):rest)
  | src == name = (name, total + val) : rest
  | otherwise = (name, total) : addSourceTotal src val rest

-- | Busca el valor asociado a un nombre en el mapa de tipos, devolviendo cero si no existe.
lookupAssignedValueOrZero :: String -> TypeMap -> Double
lookupAssignedValueOrZero name typeMap =
  case lookup name typeMap of
    Just t -> tipoValor t
    Nothing -> 0

-- | Recupera el valor numérico de una variable que debe estar definida en el mapa.
lookupAssignedValue :: String -> TypeMap -> Either String Double
lookupAssignedValue name typeMap =
  case lookup name typeMap of
    Just t -> Right (tipoValor t)
    Nothing -> Left ("La variable '" ++ name ++ "' no está asignada.")

-- | Convierte un valor de tipo a un número para comparar capacidades en validaciones.
tipoValor :: Tipo -> Double
tipoValor (Gancho e) = evalTipoValue e
tipoValor (Grillete e) = evalTipoValue e
tipoValor (Percha e) = evalTipoValue e
tipoValor (Cadena e) = evalTipoValue e
tipoValor (Sintetica e) = evalTipoValue e
tipoValor (TipoExpr e) = evalTipoValue e
tipoValor TipoVacio = 0

-- | Evalúa una expresión aritmética de tipo sin depender del estado de ejecución general.
evalTipoValue :: DoubleExp -> Double
evalTipoValue (Const valor) = valor
evalTipoValue (Var variable) = 0
evalTipoValue (UMinus e) = - evalTipoValue e
evalTipoValue (Plus e1 e2) = evalTipoValue e1 + evalTipoValue e2
evalTipoValue (Minus e1 e2) = evalTipoValue e1 - evalTipoValue e2
evalTipoValue (Times e1 e2) = evalTipoValue e1 * evalTipoValue e2
evalTipoValue (Div e1 e2) = evalTipoValue e1 / evalTipoValue e2

-- | Comprueba que cada eslinga de la lista esté declarada correctamente.
validateEslingaList :: [(String, Decl)] -> EslingaList -> Either String ()
validateEslingaList _ [] = Right ()
validateEslingaList declMap (EslingaId x:xs) = do
  case lookup x declMap of
    Just (DeclEslinga _) -> validateEslingaList declMap xs
    Just _ -> Left ("La eslinga '" ++ x ++ "' debe declararse como eslinga.")
    Nothing -> Left ("La eslinga '" ++ x ++ "' no está declarada.")
validateEslingaList declMap (EslingaNull:xs) = validateEslingaList declMap xs

-- | Valida que todas las variables de una expresión numérica hayan sido declaradas.
validateDoubleExp :: [String] -> DoubleExp -> Either String ()
validateDoubleExp declNames (Var variable) = validateVariable declNames variable
validateDoubleExp declNames (UMinus e) = validateDoubleExp declNames e
validateDoubleExp declNames (Plus e1 e2) = do validateDoubleExp declNames e1
                                              validateDoubleExp declNames e2
validateDoubleExp declNames (Minus e1 e2) = do validateDoubleExp declNames e1
                                               validateDoubleExp declNames e2
validateDoubleExp declNames (Times e1 e2) = do validateDoubleExp declNames e1
                                               validateDoubleExp declNames e2
validateDoubleExp declNames (Div e1 e2) = do validateDoubleExp declNames e1
                                             validateDoubleExp declNames e2
validateDoubleExp _ _ = Right ()

-- | Revisa expresiones booleanas comprobando que todas las variables que usan estén declaradas.
validateBoolExp :: [String] -> BoolExp -> Either String ()
validateBoolExp declNames (Eq e1 e2) = do validateDoubleExp declNames e1
                                          validateDoubleExp declNames e2
validateBoolExp declNames (Lt e1 e2) = do validateDoubleExp declNames e1
                                          validateDoubleExp declNames e2
validateBoolExp declNames (Gt e1 e2) = do validateDoubleExp declNames e1
                                          validateDoubleExp declNames e2
validateBoolExp declNames (And b1 b2) = do validateBoolExp declNames b1
                                           validateBoolExp declNames b2
validateBoolExp declNames (Or b1 b2) = do validateBoolExp declNames b1
                                          validateBoolExp declNames b2
validateBoolExp declNames (Not b) = validateBoolExp declNames b
validateBoolExp _ _ = Right ()

-- | Calcula el valor numérico de un tipo concreto con el estado actual del programa.
evalTipo :: Tipo -> State -> Double
evalTipo (Gancho e) st = evalIntExp e st
evalTipo (Grillete e) st = evalIntExp e st
evalTipo (Percha e) st = evalIntExp e st
evalTipo (Cadena e) st = evalIntExp e st
evalTipo (Sintetica e) st = evalIntExp e st
evalTipo (TipoExpr e) st = evalIntExp e st
evalTipo TipoVacio _ = 0

-- Evaluacion del comando

-- | Ejecuta un comando sobre un estado inicial vacío y sin mapa de tipos.
eval :: Comm -> State
eval p = evalComm p initState []

-- | Ejecuta un comando en un estado dado, devolviendo el estado final tras la ejecución.
evalComm :: Comm -> State -> TypeMap -> State
evalComm comm s typeMap = fst (evalCommWithTypeMap comm s typeMap)

-- | Ejecuta un comando manteniendo también el mapa de tipos para controlar capacidades y validaciones.
evalCommWithTypeMap :: Comm -> State -> TypeMap -> (State, TypeMap)
evalCommWithTypeMap Skip s typeMap = (s, typeMap)
evalCommWithTypeMap Draw s typeMap = (s, typeMap)
evalCommWithTypeMap (Let var expInt) s typeMap = (update var (evalIntExp expInt s) s, typeMap)
evalCommWithTypeMap (Each vars tipo) s typeMap = let newState = evalEach vars tipo s
                                                     newTypeMap = updateEachType vars tipo s typeMap
                                                 in (newState, newTypeMap)
evalCommWithTypeMap (Seq Skip c1) s typeMap = evalCommWithTypeMap c1 s typeMap
evalCommWithTypeMap (Seq c0 c1) s typeMap = let (s1, typeMap1) = evalCommWithTypeMap c0 s typeMap
                                             in evalCommWithTypeMap c1 s1 typeMap1
evalCommWithTypeMap (Cond b c0 c1) s typeMap = if evalBoolExp b s then evalCommWithTypeMap c0 s typeMap else evalCommWithTypeMap c1 s typeMap
evalCommWithTypeMap (Repeat c b) s typeMap = if evalBoolExp b s then evalCommWithTypeMap (Seq c (Repeat c b)) s typeMap else (s, typeMap)
evalCommWithTypeMap (Connect camino _) s typeMap = let resolvedTypeMap = map (\(var, tipo) -> (var, resolveTipo s tipo)) typeMap
                                                    in unsafePerformIO (writeFile "izaje.tex" (renderGraphWithTypes camino resolvedTypeMap) >> return (s, typeMap))

-- | Asigna a cada variable del bloque `each` el valor del tipo actual del contexto.
evalEach :: [Variable] -> Tipo -> State -> State
evalEach [] tipo s = s
evalEach (var:vars) tipo s = evalEach vars tipo (update var (evalTipo tipo s) s)

-- | Actualiza el mapa de tipos después de ejecutar un bloque `each`.
updateEachType :: [Variable] -> Tipo -> State -> [(Variable, Tipo)] -> [(Variable, Tipo)]
updateEachType [] _ _ typeMap = typeMap
updateEachType (var:vars) tipo s typeMap = updateEachType vars tipo s (updateTypeMap var (resolveTipo s tipo) typeMap)

-- Expresiones aritmeticas

-- | Evalúa una expresión aritmética sobre un estado concreto.
evalIntExp :: IntExp -> State -> Double
evalIntExp (Const valor) _ = valor
evalIntExp (Var variable) estado = lookfor variable estado
evalIntExp (UMinus expInt) estado = -(evalIntExp expInt estado)
evalIntExp (Plus exp1 exp2) estado = evalIntExp exp1 estado + evalIntExp exp2 estado
evalIntExp (Minus exp1 exp2) estado = evalIntExp exp1 estado - evalIntExp exp2 estado
evalIntExp (Times exp1 exp2) estado = evalIntExp exp1 estado * evalIntExp exp2 estado
evalIntExp (Div exp1 exp2) estado = evalIntExp exp1 estado / evalIntExp exp2 estado

-- Expresiones booleanas

-- | Evalúa una expresión booleana con la semántica del intérprete.
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

-- | Genera el código LaTeX del grafo del camino usando los tipos actuales de cada nodo.
renderGraph :: Camino -> String
renderGraph camino = renderGraphWithTypes camino []

-- | Genera el grafo con etiquetas de capacidad y posición calculadas para cada nivel.
renderGraphWithTypes :: Camino -> TypeMap -> String
renderGraphWithTypes camino typeMap =
  let lvls = levelsWithTypes camino typeMap
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

-- | Produce la lista de niveles del camino en orden top-down.
levels :: Camino -> [NodoList]
levels camino = levelsWithTypes camino []

levelsWithTypes :: Camino -> TypeMap -> [NodoList]
levelsWithTypes (CaminoPaso ns es rest) typeMap
  | isConnectorLevel es typeMap =
      let r = levelsWithTypes rest typeMap
          conn = esToNodoList es
      in case r of
           [] -> [ns, conn]
           _  -> ns : conn : r
levelsWithTypes (CaminoPaso ns es rest) typeMap
  | isPerchaLevel es =
      let r = levelsWithTypes rest typeMap
      in case r of
           [] -> [ns]
           (h:hs) -> (ns ++ h) : hs
levelsWithTypes (CaminoPaso ns _ rest) typeMap = ns : levelsWithTypes rest typeMap
levelsWithTypes (CaminoBase ns es ms) typeMap
  | isConnectorLevel es typeMap = [ns, esToNodoList es, ms]
levelsWithTypes (CaminoBase ns es ms) typeMap
  | isPerchaLevel es = [ns ++ ms]
levelsWithTypes (CaminoBase ns _ ms) _ = [ns, ms]
levelsWithTypes (CaminoFinal (NodoEslinga ns es) cargas) typeMap
  | isConnectorLevel es typeMap = [ns, esToNodoList es, cargaToNodoList cargas]
levelsWithTypes (CaminoFinal (NodoEslinga ns es) cargas) typeMap
  | null es && all (
      \n -> case n of
        NodoNull -> True
        NodoId x -> isGanchoOrGrilleteType x typeMap) ns = [ns, cargaToNodoList cargas]
levelsWithTypes (CaminoFinal (NodoEslinga ns es) cargas) typeMap
  | isPerchaLevel es = [ns ++ cargaToNodoList cargas]
levelsWithTypes (CaminoFinal (NodoEslinga ns _ ) cargas) _ = [ns, cargaToNodoList cargas]
levelsWithTypes (CaminoFinal (EslingaNodo es ns) cargas) typeMap
  | isConnectorLevel es typeMap = [ns, esToNodoList es, cargaToNodoList cargas]
levelsWithTypes (CaminoFinal (EslingaNodo es ns) cargas) typeMap
  | null es && all (
      \n -> case n of
        NodoNull -> True
        NodoId x -> isGanchoOrGrilleteType x typeMap) ns = [ns, cargaToNodoList cargas]
levelsWithTypes (CaminoFinal (EslingaNodo es ns) cargas) typeMap
  | isPerchaLevel es = [ns ++ cargaToNodoList cargas]
levelsWithTypes (CaminoFinal (EslingaNodo _ ns) cargas) _ = [ns, cargaToNodoList cargas]

-- | Detecta si un nivel corresponde a una percha, es decir, no tiene eslingas relevantes.
isPerchaLevel :: EslingaList -> Bool
isPerchaLevel es = null es || all (== EslingaNull) es

-- | Detecta si una lista de eslingas representa un nivel de conectores gancho/grillete que debe dibujarse como nodos.
isConnectorLevel :: EslingaList -> TypeMap -> Bool
isConnectorLevel es typeMap =
  not (null es) && all isConnectorEslinga es
  where
    isConnectorEslinga EslingaNull = True
    isConnectorEslinga (EslingaId x) = isGanchoOrGrilleteType x typeMap

-- | Convierte una lista de eslingas de tipo conector en nodos para el layout del grafo.
esToNodoList :: EslingaList -> NodoList
esToNodoList [] = []
esToNodoList (EslingaNull:xs) = NodoNull : esToNodoList xs
esToNodoList (EslingaId x:xs) = NodoId x : esToNodoList xs

-- | Asigna coordenadas bidimensionales a cada nodo según su nivel y posición horizontal.
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

-- | Genera la línea del vértice de TikZ y, si corresponde, la etiqueta de capacidad del nodo.
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

-- | Formatea la capacidad para que pueda mostrarse como etiqueta numérica en el grafo.
formatCapacity :: Tipo -> String
formatCapacity t = formatCoord (tipoValor t)

-- | Reduce un valor flotante a una representación legible en LaTeX sin decimales innecesarios.
formatCoord :: Double -> String
formatCoord v = if abs (v - fromInteger (round v)) < 1e-6
                then show (round v)
                else show v

-- | Obtiene la lista plana de nodos del camino para su posterior renderización.
nodesInCamino :: Camino -> [String]
nodesInCamino (CaminoBase ns _ ms) = map showNodo (ns ++ ms)
nodesInCamino (CaminoPaso ns _ rest) = map showNodo ns ++ nodesInCamino rest

-- | Construye las aristas del camino a partir de sus niveles y eslingas.
edgesInCamino :: Camino -> [(String, String, String)]
edgesInCamino camino = edgesInCaminoWithTypes camino []

-- | Genera todas las conexiones entre nodos, incluyendo etiquetas de eslinga y perchas horizontales.
edgesInCaminoWithTypes :: Camino -> TypeMap -> [(String, String, String)]
edgesInCaminoWithTypes (CaminoPaso ns es rest) typeMap
  | isConnectorLevel es typeMap =
      let connNodes = esToNodoList es
          nextNodes = firstNodes rest
          perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
          connectorEdges = directNodeLevelLinks ns connNodes ++ directNodeLevelLinks connNodes nextNodes
      in perchaEdges ++ connectorEdges ++ edgesInCaminoWithTypes rest typeMap
edgesInCaminoWithTypes (CaminoPaso ns es rest) typeMap =
  let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
      lowerEdges = if isPerchaLevel es then [] else linkNodes ns (firstNodes rest) es typeMap
  in perchaEdges ++ lowerEdges ++ edgesInCaminoWithTypes rest typeMap

edgesInCaminoWithTypes (CaminoBase ns es ms) typeMap
  | isConnectorLevel es typeMap =
      let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
          connNodes = esToNodoList es
          lowerEdges = directNodeLevelLinks ns connNodes ++ directNodeLevelLinks connNodes ms
      in perchaEdges ++ lowerEdges
edgesInCaminoWithTypes (CaminoBase ns es ms) typeMap =
  let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
      lowerEdges = if isPerchaLevel es then [] else linkNodes ns ms es typeMap
  in perchaEdges ++ lowerEdges

edgesInCaminoWithTypes (CaminoFinal (NodoEslinga ns es) cargas) typeMap
  | isConnectorLevel es typeMap =
      let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
          finalNodes = cargaToNodoList cargas
          connNodes = esToNodoList es
      in perchaEdges ++ directNodeLevelLinks ns connNodes ++ directNodeLevelLinks connNodes finalNodes
edgesInCaminoWithTypes (CaminoFinal (NodoEslinga ns es) cargas) typeMap
  | null es && all (\n -> case n of NodoNull -> True; NodoId x -> isGanchoOrGrilleteType x typeMap) ns =
      let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
          finalNodes = cargaToNodoList cargas
      in perchaEdges ++ directNodeLevelLinks ns finalNodes
edgesInCaminoWithTypes (CaminoFinal (NodoEslinga ns es) cargas) typeMap =
  let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
      finalNodes = cargaToNodoList cargas
      lowerEdges = if isPerchaLevel es then [] else linkNodes ns finalNodes es typeMap
  in perchaEdges ++ lowerEdges

edgesInCaminoWithTypes (CaminoFinal (EslingaNodo es ns) cargas) typeMap
  | isConnectorLevel es typeMap =
      let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
          finalNodes = cargaToNodoList cargas
          connNodes = esToNodoList es
      in perchaEdges ++ directNodeLevelLinks ns connNodes ++ directNodeLevelLinks connNodes finalNodes
edgesInCaminoWithTypes (CaminoFinal (EslingaNodo es ns) cargas) typeMap
  | null es && all (\n -> case n of NodoNull -> True; NodoId x -> isGanchoOrGrilleteType x typeMap) ns =
      let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
          finalNodes = cargaToNodoList cargas
      in perchaEdges ++ directNodeLevelLinks ns finalNodes
edgesInCaminoWithTypes (CaminoFinal (EslingaNodo es ns) cargas) typeMap =
  let perchaEdges = if arePerchaNodes ns typeMap then horizontalPerchaByType ns typeMap else []
      finalNodes = cargaToNodoList cargas
      lowerEdges = if isPerchaLevel es then [] else linkNodes ns finalNodes es typeMap
  in perchaEdges ++ lowerEdges

-- | Devuelve el primer nivel del camino para vincular nodos entre niveles.
firstNodes :: Camino -> NodoList
firstNodes (CaminoPaso ns _ _) = ns
firstNodes (CaminoBase ns _ _) = ns
firstNodes (CaminoFinal (NodoEslinga ns _) _) = ns
firstNodes (CaminoFinal (EslingaNodo _ ns) _) = ns

-- | Conecta dos niveles de nodos usando la lista de eslingas correspondiente.
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

-- | Conecta dos niveles de nodos directamente por índice, útil cuando una lista de conectores se dibuja como nivel real.
directNodeLevelLinks :: NodoList -> NodoList -> [(String, String, String)]
directNodeLevelLinks prev next =
  let prevNames = map showNodo (filter (not . isNullNodo) prev)
      nextNames = map showNodo (filter (not . isNullNodo) next)
  in if null prevNames || null nextNames
       then []
       else zipWith (\a b -> (a, b, "")) (padNames prevNames (length nextNames)) (padNames nextNames (length prevNames))
  where
    padNames names n
      | n <= length names = names
      | otherwise = names ++ replicate (n - length names) "null"

-- | Forma la etiqueta visible de una eslinga en el grafo, incluyendo su capacidad.
eslingaLabel :: String -> TypeMap -> String
eslingaLabel "null" _ = ""
eslingaLabel name typeMap =
  case lookup name typeMap of
  Just t -> name ++ " " ++ formatCapacity t ++ "tn"
  Nothing -> name

-- | Calcula el índice del nodo destino correspondiente a cada eslinga para mantener la relación entre niveles.
nodeIndex :: Int -> Int -> Int -> Int
nodeIndex idx total len
  | total <= 1 = 0
  | len <= 1 = 0
  | otherwise = round (fromIntegral idx * fromIntegral (total - 1) / fromIntegral (len - 1))

-- | Crea conexiones horizontales entre nodos que pertenecen a una misma percha.
horizontalPercha :: NodoList -> [(String, String, String)]
horizontalPercha xs =
  let names = map showNodo (filter (not . isNullNodo) xs)
  in case names of
       [] -> []
       [_] -> []
       _ -> zipWith (\a b -> (a, b, "")) (init names) (drop 1 names)

-- | Determina si todos los nodos de una lista son perchas de tipo compatible.
arePerchaNodes :: NodoList -> TypeMap -> Bool
arePerchaNodes xs typeMap =
  let names = [x | NodoId x <- filter (not . isNullNodo) xs]
  in not (null names) && all (\x -> isPerchaType x typeMap) names

-- | Produce conexiones horizontales sólo cuando la lista de nodos constituye una percha válida.
horizontalPerchaByType :: NodoList -> TypeMap -> [(String, String, String)]
horizontalPerchaByType xs typeMap =
  if arePerchaNodes xs typeMap
    then horizontalPercha xs
    else []

-- | Renderiza una arista con la etiqueta de la eslinga y la posición adecuada para el layout.
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

-- | Comprueba si un nodo es un placeholder nulo que debe ignorarse al dibujar.
isNullNodo :: Nodo -> Bool
isNullNodo NodoNull = True
isNullNodo _ = False

-- | Convierte un nodo a su representación textual para identificarlo en el grafo.
showNodo :: Nodo -> String
showNodo (NodoId x) = x
showNodo NodoNull = "null"

-- | Convierte una eslinga a su representación textual para visualizarla en el grafo.
showEslinga :: Eslinga -> String
showEslinga (EslingaId x) = x
showEslinga EslingaNull = "null"
