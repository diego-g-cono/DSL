module Main where

import System.Environment (getArgs)
import Parser (parseProgram)
import Eval1 (evalProgram)
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do arg:_ <- getArgs
          run arg

run :: [Char] -> IO ()
run ifile = do
  s <- readFile ifile
  case parseProgram ifile s of
    Left error -> print error
    Right t    -> do print (evalProgram t)
                     putStrLn "Archivo izaje.tex generado con el grafo del sistema."
