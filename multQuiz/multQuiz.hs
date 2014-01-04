module Main
  where

import System.IO
import System.Random
import System.Time
import System.Environment
import Data.List

{-

MultQuiz program.  quiz the user on multiplying pairs of numbers.  Keep statistics on the replies, and 
select the next pair of numbers based on those statistics.

syntax:

MultQuiz a b statsfile == generate a blank statistics file with all the pairs (a to b, a to b)
MultQuiz statsfile == play the game using the passed statsfile.  
MultQuiz <anything else> == print syntax message and exit.
-}

{-
 Load test stuff!
 statsfile <- (readFile "1to10.stats")
 let stats = (fst (head (reads statsfile :: [([(Stat Int)], String)])))
-}

-- make stats a list.  Each entry in the list is of the form:
-- [(x, y), (bool, timediff), (bool, timediff)... ]

-- A 'Stat' is a pair of numbers with a bool and timediff for each 
-- user guess.  The bool indicates whether the user guessed correctly
-- The timediff is how long it took to guess.
data Stat a = Stat (a, a) [(Bool, Double)]
  deriving (Show, Read)

{-
instance Functor Stat where
   fmap f (Stat (x,y) sts) = Stat (f x, f y) sts
-}

sLeft (Stat (a, b) list) = a
sRight (Stat (a, b) list) = b

{-
readStats :: (Stat a) => String -> a
readStats instring = file
 read instring
-}

addToStat success seconds (Stat (a, b) []) = 
  Stat (a,b) [(success, seconds)]

addToStat success seconds (Stat (a, b) (x:y)) = 
  Stat (a, b) ((success, seconds) : x : y)

addStats (a, b) success seconds ((Stat (x,y) sl) : ss) = 
  if (a, b) == (x, y) then
    (addToStat success seconds (Stat (x,y) sl)) : ss
  else
    (Stat (x,y) sl) : (addStats (a, b) success seconds ss)

addStats (a, b) success seconds [] = 
   [Stat (a, b) [(success, seconds)]]

-- Figure out which one to quiz on next.

calcTehScore ((success, seconds):rest) (count, score) = 
  if (success == True)
   then
     calcTehScore rest (count + 1, score + seconds)
   else
     calcTehScore rest (count + 1, score + seconds * 2.0)

calcTehScore [] (count, score) = 
  if (count == 0)
   then 100
   else score / count

calcScore (Stat (a, b) sl) = 
  calcTehScore sl (0, 0.0)

seqlist count list = 
  if null list then
    []
  else
    count : seqlist (count + 1) (tail list) 

--pickIndex :: [Stat a] -> IO Int

pickIndex stats random = 
 let sorted = reverse (sort (zip (map calcScore stats) (seqlist (0::Int) stats)))
  in
   (snd (sorted !! random))

sortedstats stats = 
 let sorted = reverse (sort (zip (map calcScore stats) (seqlist (0::Int) stats)))
  in
   map (\x -> (stats !! (snd x))) sorted

printIndex stats count = 
  if (null stats) || (count == 0) then 
     putStr ""
   else do
    putStr (show (calcScore (head stats)))
    printStat (head stats)
    printIndex (tail stats) (count - 1)
    
  --  do
      -- tehindex <- randomRIO (0::Int, (quot (length sorted) 5)::Int)
      -- putStr "that score: "
      -- putStrLn (show tehindex)
      --randomRIO (0::Int, (quot (length sorted) 5)::Int)
      
{-

testfun = 
 let leest = [1,3,4]
  in 
   do 
    random <- randomRIO (0,2)
    (leest !! random)

pickIndex stats = 
 let sorted = reverse (sort (zip (map calcScore stats) (seqlist 0 stats)))
  in
   if null sorted 
    then 0
    else 
     do
      tehindex <- randomRIO (0::Int, quot (length sorted) 5)
      putStr "that score: "
      putStrLn (show tehindex)
      (snd (sorted !! tehindex))
-}
--     (snd (sorted !! 0))

-- Some time related ftns:

picoCorrect :: TimeDiff -> TimeDiff
picoCorrect td = 
 if (tdPicosec td) < 0
  then 
   picoCorrect TimeDiff {
	    tdYear    = (tdYear td),
	    tdMonth   = (tdMonth td),
	    tdDay     = (tdDay td),
	    tdHour    = (tdHour td),
	    tdMin     = (tdMin td),
	    tdSec     = (tdSec td) - 1,
	    tdPicosec = (tdPicosec td) + 1000000000000
	    }
  else
   td

-- picoCorrect td = td

toSeconds :: ClockTime -> Double
toSeconds (TOD x y) = 
  fromInteger x +  (fromInteger y) / 1000000000000.0

tdToSeconds :: TimeDiff -> Double
tdToSeconds td = 
 toSeconds (addToClockTime (picoCorrect td) (TOD 0 0))

-- Print the stats.  Only the non-null ones though.

printThoseSortedStats stats = 
 let { sorted = reverse (sort (zip (map calcScore stats) (seqlist 0 stats))) ;
  sortstats = map (\(score, index) -> (score, stats !! index) ) sorted }
  in
   printSortedStats sortstats

printSortedStats ((score, (Stat (x,y) sl)) : ss) = 
 if (null sl)
  then printSortedStats ss
  else do
   putStr (show score)
   putStr " - ("
   putStr (show x)
   putStr ","
   putStr (show y)
   putStr ") "
   printStatList sl
   printSortedStats ss

printSortedStats [] = do 
 putStrLn ""

{-
printStats ((Stat (x,y) sl) : ss) = 
 if (null sl)
  then printStats ss
  else do
   putStr "("
   putStr (show x)
   putStr ","
   putStr (show y)
   putStr ") "
   printStatList sl
   printStats ss

printStats [] = do 
 putStrLn ""
-}

printStat (Stat (x,y) sl) = do
   putStr "("
   putStr (show x)
   putStr ","
   putStr (show y)
   putStr ") "
   printStatList sl


-- Print the list of (bool, TimeDiff) built in to each Stat obj.
printStatList ((a, b) : rest) = do
 putStr (show a)		-- bool
 putStr " "
 putStr (show b)  -- time in seconds
 putStr " "
 printStatList rest

printStatList [] = 
 putStrLn ""

--listem :: a -> a -> [(a, a)] -> [(a, a)]

listem low high = 
 let
 {
  listem left low high leest = 
   if (left > high) then
    leest
   else
    (listm left low high leest) ++ (listem (left + 1) low high leest)
  ;
  listm left low high leest = 
     if (low == high) 
      then
       (left, low) : leest
      else
       (left, low) : (listm left (low+1) high leest)
 }
 in 
  listem low low high []

makestat (a,b) = (Stat (a,b) [])

main = do
  hSetBuffering stdin LineBuffering
  args <- getArgs
  if (length args) == 1 then do
    statsfile <- readFile (head args) ;
    putStrLn "Welcome to the multquiz.  Enter 'q' at any time to quit."
    doGuessing 
     (fst (head (reads statsfile :: [([(Stat Int)], String)])))
     (head args)
   else if (length args) == 3 then
    let tehstats = (map makestat (listem (read (head args) :: Int) (read (args !! 1) :: Int))) 	
     in do
       putStrLn "Welcome to the multquiz.  Enter 'q' at any time to quit.";
       doGuessing tehstats (args !! 2)
   else do
    print "MultQuiz a b statsfile == generate a blank statistics file with all the pairs (a to b, a to b)"
    print "MultQuiz statsfile == play the game using the passed statsfile.  "
    print "MultQuiz <anything else> == print syntax message and exit."

       
{-    
   
  putStrLn "Welcome to the multquiz.  Enter 'q' at any time to quit."
--  doGuessing (map makestat (listem 11 99))
  statsfile <- readFile "stats"
  doGuessing (fst (head (reads statsfile :: [([(Stat Int)], String)])))st
-- doGuessing []

-}

doGuessing :: [(Stat Int)] -> String -> IO ()
doGuessing stats statsfilename = do
   index <- randomRIO (0::Int, quot (length stats) 5)
   --putStr "Index: "
   --putStrLn (show index)
   --printIndex (sortedstats stats) (index + 1)
   --let index = pickIndex stats in
   --index <- pickIndex stats
   tryGuessing 
     (sLeft ((sortedstats stats) !! index)) 
     (sRight ((sortedstats stats) !! index)) 
     stats statsfilename

tryGuessing :: Int -> Int -> [(Stat Int)] -> String -> IO ()
tryGuessing left right stats statsfilename = do
  putStr "What's " 
  putStr (show left)
  putStr " times "
  putStr (show right)
  putStrLn "?"
  putStrLn "Enter your guess: "
  btime <- getClockTime
  guess <- getLine
  if guess == "q" then do
    printThoseSortedStats stats
    writeFile statsfilename (show stats)
   else do
     let guessNum = read guess
     etime <- getClockTime
     let seconds = tdToSeconds(diffClockTimes etime btime)
     if guessNum == left*right
       then do 
         putStrLn "Yep!"
         doGuessing (addStats (left, right) True seconds stats) statsfilename
       else do 
         putStrLn "Nope! - try again: "
	 tryGuessing left right (addStats (left, right) False seconds stats) statsfilename

