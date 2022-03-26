module Main where

import System.Environment ( getArgs )
import Solve

main :: IO ()
main = play

play :: IO ()
play = do
    let toLower x = case lookup x $ zip ['A'..'Z'] ['a'..'z'] of Just lc -> lc; Nothing -> x

    args <- fmap ((fmap . fmap) toLower) getArgs 
    let f s = initialSolver s  >>= solveTheGame
    case args of
        []         -> putStrLn "Naive Wordle solver!"  >> f Naive 
        ["naive"]  -> putStrLn "Naive Wordle solver!"  >> f Naive 
        ["clever"] -> putStrLn "Clever Wordle solver!" >> f Clever
        _ -> putStrLn "Invalid arguments, syntax  should be: $  stack exec solver-exe [naive | clever]"   