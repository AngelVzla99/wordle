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
-- Auxiliar functions           |
---------------------------------

pickRandomList :: [a] -> IO a
pickRandomList l = (l !!) <$> randomRIO (0, length l - 1)

boolToInt :: Bool -> Int 
boolToInt True = 1
boolToInt _ = 0 

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
        f s n = do
            ms <-  getHint n
            newState <- updateState ms s
            print newState
            when (remaining newState == 1) mzero
            pure newState
    
    foldM_ f s [1..turns] 
    putStrLn "You Lost \129319"
    pure ()

updateState :: [Match] -> SolverState -> IO SolverState
updateState ms gs@GS {strategy=_strategy} = case _strategy of
    Naive  -> naive ms gs 
    Clever -> pure $ clever ms gs

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

goodMatch :: (String,[Match]) -> Bool 
goodMatch (str,m) = foldr condition True $ zip m str
    where
        arbol = AA.fromList $ map (\x -> (x,True)) str
        condition :: (Match,Char) -> Bool -> Bool
        condition (m,c) prev = case m of
            Correct c2 -> prev && (c2==c) 
            Misplaced c2 -> prev && (c2/=c) && (AA.member c2 arbol) 
            Absent c2 -> prev && not (AA.member c2 arbol) 

sieve :: [Match] -> [String] -> [String]
sieve m strs = map fst $ filter goodMatch $ map addTo strs
    where addTo x = (x,m) 

naive :: [Match] -> SolverState -> IO SolverState 
naive m (GS sug pos rem dic stra) = do 
    let newPossible = sieve m pos 
    newSuggestion <- pickRandomList newPossible
    return $ GS newSuggestion newPossible (length newPossible) dic stra 

clever :: [Match] -> SolverState -> SolverState
clever m (GS sug pos rem dic stra) 
    = (GS newSuggestion newPossible (length newPossible) dic stra)
    where
        newPossible = sieve m pos 
        dup x = (x,x)
        treePossible = AA.fromList $ map dup pos 
        -- Esto se puede hacer sin pasar 'possible' a arbol, pero como lo pide el enunciado ...
        newSuggestion = snd $ minimum $ countIfTarget treePossible

countIfTarget :: (Foldable f, Functor f) => f String -> f (Int, String)
countIfTarget arbol = fmap countRest arbol 
    where 
        countRest :: String -> (Int,String)
        countRest str = foldr sumarTree (0,str) arbol
        sumarTree :: String -> (Int,String) -> (Int,String)
        sumarTree cuStr (suma,trg) = ( suma + (allIn cuStr trg) , trg ) 
        allIn :: String -> String -> Int 
        allIn str1 str2 = boolToInt $ all (\x -> elem x str2) str1

