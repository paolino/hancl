module Main where

import System.Random            
import System.IO
import System.Environment
import Net.Net  as Net
import Net.Parse

main = do
  args <- getArgs 
  generator <- getStdGen
  datas <- readFile $ args !! 0
  config <- readFile $ args !! 1
  let convergence = read $ args !! 2 
      cycle       = read $ args !! 3 
      (nodes,ls) =  parseAndRun config datas generator (evolution cycle convergence) 
  hPrint stderr $ ls
  putStr $ nodes
