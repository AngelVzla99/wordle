{-|
Module      : Solve
Description : Provides the main logic of the solver, its strategies and whatnot.
License     : GPL-3
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve
Stability   : experimental
Portability : POSIX
-}

module Solve where

import AA (AA)
import qualified AA 
import Util 
import Match 
import Data.Functor 
import Data.Bifunctor
import Control.Monad
import System.Random
import Text.Read
import Control.Applicative
import System.Environment
import Data.Char
import System.IO

---------------------------------
-- Types                        |
---------------------------------

data Solver = Naive | Clever

data SolverState = GS 
    { suggestion :: String
    , possible :: [String]
    , remaining :: Int
    , dict :: AA.AA String String
    , strategy :: Solver
    }


---------------------------------
-- Instances                    |
---------------------------------

instance Show SolverState where
    show GS {suggestion=_suggestion,remaining=1}
        = "It must be \171" ++  _suggestion ++ "\187."
    show GS {suggestion=_suggestion,remaining=_remaining}
        = show _remaining ++ " words remain. I suggest: \171" ++  _suggestion ++ "\187."


---------------------------------
-- Functions                    |
---------------------------------

play :: IO ()
play = do
    args <- fmap ((fmap . fmap) toLower) getArgs 
    let f s = initialSolver s  >>= solveTheGame
    case args of
        []         -> putStrLn "Naive Wordle solver!"  >> f Naive 
        ["naive"]  -> putStrLn "Naive Wordle solver!"  >> f Naive 
        ["clever"] -> putStrLn "Clever Wordle solver!" >> f Clever
        _ -> putStrLn "Invalid arguments, syntax  should be: $  stack exec solver-exe [naive | clever]"    


infixr 9 |>
(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

initialSolver :: Solver -> IO SolverState
initialSolver strat 
    = loadDictionary dictionary 
    <&> diag 
    |> \ (r,d) -> GS 
        { suggestion=mempty
        , possible=[]
        , remaining=r
        , dict=d
        , strategy=strat}
    where
        diag :: AA a b -> (Int, AA a b)
        diag d = (length d,d)

solveTheGame :: SolverState -> IO ()
solveTheGame gs = (solveTheGame' gs >> recurse) <|> recurse 
    where
        recurse = yesOrNo "Solve Another" >>= \b -> when b (solveTheGame gs) 

solveTheGame' :: SolverState -> IO ()
solveTheGame' s'@GS {remaining=_remaining} = do
    putStrLn $ "There are " ++ show _remaining ++ " possible words."
    let s = s'{possible= fmap snd $ AA.toList $ dict s}
    foldM_ (\s n -> getHint n >>= \m -> suggestRound m s) s [1..turns] 
    putStrLn "You Lost \129319"
    pure ()

updateState :: [Match] -> SolverState -> IO SolverState
updateState ms gs@GS {strategy=_strategy} = case _strategy of
    Naive  -> naive ms gs 
    Clever -> pure $ clever ms gs

suggestRound :: [Match] -> SolverState -> IO SolverState
suggestRound ms s@GS {suggestion=sol,remaining=n}
    | n == 1    = putStrLn ("It must be \171" ++ sol ++ "\182") >> mzero 
    | otherwise = updateState ms s >>= \s -> print s >> pure s

getHint :: Int -> IO [Match]
getHint n = do 
    putStr ("Hint " ++ show n ++ ' ' : carita ++ " ? ")
    hFlush stdout
    uInput <- getLine
    case readMaybe uInput :: Maybe [Match] of
        Just m -> pure m
        _      -> putStrLn uInput >> getHint n
    where
        genCarita 6 = "\128556"
        genCarita 5 = "\128533"
        genCarita _ = "\129300"

        carita = genCarita n
