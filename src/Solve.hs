{-|
Module      : Solve
Description : Provides the main logic of the solver, its strategies and whatnot.
License     : GPL-3
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve
Stability   : experimental
Portability : POSIX
-}

module Solve
    ( initialSolver
    , solveTheGame
    , Solver(..)
    )
    where

import AA (AA)
import qualified AA 
import Util ( dictionary, loadDictionary, turns, yesOrNo )
import Match ( Match(..) ) 
import Data.Functor ( (<&>) ) 
import Control.Monad ( foldM_, when, MonadPlus(mzero) )
import Text.Read ( readMaybe )
import Control.Applicative ( Alternative((<|>)) )
import Data.Char ( toLower )
import System.IO ( hFlush, stdout )
import System.Random ( Random(randomRIO) )

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

-- | Pick a random element in a list with uniform probability
pickRandomList :: [a] -> IO a
pickRandomList l = (l !!) <$> randomRIO (0, length l - 1)

-- | Transform bool to int
boolToInt :: Bool -> Int 
boolToInt True = 1
boolToInt _ = 0 

---------------------------------
-- Functions                    |
---------------------------------

infixr 9 |>
(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

-- | Return The initial state for the solver program
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

-- | This is the main function for the solver, it contain a recursive 
-- | call for every match selected by the user
solveTheGame :: SolverState -> IO ()
solveTheGame gs = (solveTheGame' gs >> recurse) <|> recurse 
    where
        recurse = yesOrNo "Solve Another" >>= \b -> when b (solveTheGame gs) 

-- | Auxiliar function for solveTheGame, here is all the logic
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

-- | This function update the current game state with a list of 
-- | Match filtering the word that don't satify the [match] conditions
updateState :: [Match] -> SolverState -> IO SolverState
updateState ms gs@GS {strategy=_strategy} = case _strategy of
    Naive  -> naive ms gs 
    Clever -> pure $ clever ms gs

-- | Function to print the hint to the user 
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

-- | Function to know if a word in dic satisfy the conditions of the
-- | list of matchs
goodMatch :: (String,[Match]) -> Bool 
goodMatch (str,m) = foldr condition True $ zip m str
    where
        arbol = AA.fromList $ map (\x -> (x,True)) str
        condition :: (Match,Char) -> Bool -> Bool
        condition (m,c) prev = case m of
            -- If the character in match is c2 then the character in word have to be the same
            Correct c2 -> prev && (c2==c) 
            -- If a Misplaced happend, the character in the word have to be different 
            -- and c2 have to be in the string
            Misplaced c2 -> prev && (c2/=c) && (AA.member c2 arbol) 
            -- If c2 is absent then, it can't be in the input stirng
            Absent c2 -> prev && not (AA.member c2 arbol) 

-- | Make a filter in a list of word, leaving only the words that
-- | satisfy the conditions of a list of matchs
sieve :: [Match] -> [String] -> [String]
sieve m strs = map fst $ filter goodMatch $ map addTo strs
    where addTo x = (x,m) 

-- | Take a list of match and in base on them make a filter in the
-- | list of words of the current state, and pick a random word between them
naive :: [Match] -> SolverState -> IO SolverState 
naive m (GS sug pos rem dic stra) = do 
    let newPossible = sieve m pos 
    newSuggestion <- pickRandomList newPossible
    return $ GS newSuggestion newPossible (length newPossible) dic stra 

-- | Take a list of match and in base on them make a filter in the
-- | list of words of the current, state and pick the a word in that new
-- | list such that minimize the remaining possible words
clever :: [Match] -> SolverState -> SolverState
clever m (GS sug pos rem dic stra) 
    = (GS newSuggestion newPossible (length newPossible) dic stra)
    where
        newPossible = sieve m pos 
        dup x = (x,x)
        treePossible = AA.fromList $ map dup newPossible 
        -- This can be done without tranforming 'possible' to tree, but as the statement asks it...
        newSuggestion = snd $ minimum $ countIfTarget treePossible

-- | This function take a list of word and for each one of them 
-- | return a pair (numberOfRemainingWord,theWord)
countIfTarget :: (Foldable f, Functor f) => f String -> f (Int, String)
countIfTarget arbol = fmap countRest arbol 
    where 
        countRest :: String -> (Int,String)
        countRest str = foldr sumarTree (0,str) arbol
        sumarTree :: String -> (Int,String) -> (Int,String)
        sumarTree cuStr (suma,trg) = ( suma + (allIn cuStr trg) , trg ) 
        allIn :: String -> String -> Int 
        allIn str1 str2 = boolToInt $ all (\x -> elem x str2) str1

