module Eval1 (eval, evalProgram, renderGraph) where

import AST
import Data.List (nub)
import System.IO (writeFile)
import System.IO.Unsafe (unsafePerformIO)

-- Estado para valores numericos del DSL simple.
type State = [(Variable, Double)]

initState :: State
initState = []

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
evalProgram (Program _ asigs comm) =
  let st0 = foldl evalAsig initState asigs
  in evalComm comm st0

evalAsig :: State -> Asig -> State
evalAsig st (Asig var tipo) = update var (evalTipo tipo st) st

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
eval p = evalComm p initState

evalComm :: Comm -> State -> State
evalComm Skip s = s
evalComm Draw s = s
evalComm (Let var expInt) s = update var (evalIntExp expInt s) s
evalComm (Seq Skip c1) s = evalComm c1 s
evalComm (Seq c0 c1) s = evalComm (Seq Skip c1) (evalComm c0 s)
evalComm (Cond b c0 c1) s = if evalBoolExp b s then evalComm c0 s else evalComm c1 s
evalComm (Repeat c b) s = if evalBoolExp b s then evalComm (Seq c (Repeat c b)) s else s
evalComm (Connect camino _) s = unsafePerformIO (do writeFile "izaje.tex" (renderGraph camino)
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
renderGraph camino =
  let lvls = levels camino
      numLvls = length lvls
      gapY = 2.0
      spacingX = 4.0
      -- flatten nodes preserving order (as names)
      allNodes = nub (map showNodo (concat lvls))
      -- map node name to coordinates
      coords = assignCoords lvls numLvls spacingX gapY
      vertexLines = map (renderVertex coords) allNodes
      edgeLines = map renderEdge (edgesInCamino camino)
  in unlines ( [ "\\begin{tikzpicture}",
                 "\\GraphInit[vstyle=Normal]",
                 "\\SetGraphUnit{4}" ]
               ++ vertexLines
               ++ edgeLines
               ++ [ "\\end{tikzpicture}" ] )

-- Produce list of levels (top-down), each level is a NodoList
levels :: Camino -> [NodoList]
levels (CaminoPaso ns _ rest) = ns : levels rest
levels (CaminoBase ns _ ms) = [ns, ms]

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
      in zip (map showNodo nodes) (zip xs (repeat y))

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
edgesInCamino (CaminoPaso ns es rest) = linkNodes ns (firstNodes rest) es ++ edgesInCamino rest
edgesInCamino (CaminoBase ns es ms) = linkNodes ns ms es


firstNodes :: Camino -> NodoList
firstNodes (CaminoPaso ns _ _) = ns
firstNodes (CaminoBase ns _ _) = ns

linkNodes :: NodoList -> NodoList -> EslingaList -> [(String, String, String)]
linkNodes [] _ _ = []
linkNodes _ [] _ = []
linkNodes prev next es =
  let prevNames = map showNodo prev
      nextNames = map showNodo next
      edgeNames = map showEslinga es
      pairs = zipWith (\a b -> (a, b)) (take (length edgeNames) (cycle prevNames)) (take (length edgeNames) (cycle nextNames))
  in zipWith (\(a, b) label -> (a, b, label)) pairs edgeNames

renderEdge :: (String, String, String) -> String
renderEdge (a, b, label) = "\\Edge[label={" ++ label ++ "}]({" ++ a ++ "})({" ++ b ++ "})"

showNodo :: Nodo -> String
showNodo (NodoId x) = x
showNodo NodoNull = "null"

showEslinga :: Eslinga -> String
showEslinga (EslingaId x) = x
showEslinga EslingaNull = "null"
