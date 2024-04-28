import Control.Monad
import Data.List
import System.Environment
import System.Random

{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}

-- main = do
--   args <- getArgs
--   unless (null args) $ do
--     if head args == "-n"
--       then do
--         putStr $ unwords $ tail args
--       else do putStrLn $ unwords args
main = do
  gen <- getStdGen
  putStr $ show $ lottery gen

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}

-- better: use nub
lottery :: StdGen -> [Int]
lottery gen = sort (take 6 (infiniteNonRepeating (1, 49) gen))

-- 😏
infiniteNonRepeating :: (Int, Int) -> StdGen -> [Int]
infiniteNonRepeating b g = infiniteNonRepeating' [] g
  where
    infiniteNonRepeating' p g = let (v, ng) = drawFromRangeExcluding b p g in v : infiniteNonRepeating' (v : p) ng

drawFromRangeExcluding :: (Int, Int) -> [Int] -> StdGen -> (Int, StdGen)
drawFromRangeExcluding b p g =
  if randint `elem` p
    then drawFromRangeExcluding b p ng
    else (randint, ng)
  where
    (randint, ng) = randomR b g
