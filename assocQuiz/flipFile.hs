module Main
  where

import System.IO
import System.Random
import System.Environment
import System.Time
import Data.List


stringlist :: [Char] -> [Char] -> [[Char]]
stringlist (a:b) c = 
 if (a == '\n') then
   c : (stringlist b [])
  else
   stringlist b (c++[a])

stringlist [] c = [c]

pairz (a:b:c) = (a, b) : pairz c
pairz [a] = []
pairz [] = []

printEm ((a,b): c) = do
  putStrLn b
  putStrLn a
  printEm c

printEm [] = return ()

main = do
  hSetBuffering stdin LineBuffering
  args <- getArgs
  if (length args) == 1 then do
    pairsfile <- readFile (head args) ;
    let pairs = pairz (stringlist pairsfile []) in 
      printEm pairs
   else do
    print "Flipfile infile == write another file with all the lines interchanged."

       
