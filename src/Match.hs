{-|
Module      : Match
Description : Provides a way to check via a data type.
License     : GPL-3
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve
Stability   : experimental
Portability : POSIX
-}
module Match where


import qualified AA
import qualified Text.Read as Text
import Text.ParserCombinators.ReadPrec ( look, pfail, get, ReadPrec )
import Data.Bifunctor ( Bifunctor(bimap) ) 

------------------------------
-- Types                     |
------------------------------

-- | Represents the target word, that is, the word that is the solution.
newtype Target = T {getTarget :: String}

-- | Represents the guess word, that is, the word that the user input.
newtype Guess  = G {getGuess  :: String}


-- | Represents all the possible matches for a letter.
data Match 
    = Absent    Char -- ^ The letter never appears in target
    | Misplaced Char -- ^ The letter appears somewhere in target (but it's misplaced)
    | Correct   Char -- ^ The letter appears exactly in the same position in target.
    deriving Eq
------------------------------
-- Instances                 |
------------------------------

instance Show Match where 
    show (Absent c)    = ['\11035',c]
    show (Misplaced c) = ['\129000',c] 
    show (Correct c)   = ['\129001',c]

instance Show Target where
    show T {getTarget=t} = "It was " ++ t

instance Show Guess where
    show G {getGuess=g}  = "Your guess: " ++ g

-- Our read instance is forgiving, it does not care for spaces nor parenthesis.
instance Read Match where
    readPrec     =  Text.parens $ do
        skipSpaces
        color <- get 
        skipSpaces
        c     <- get
        skipSpaces
        
        case color of
            '\11035'  -> return (Absent c)
            '\129000' -> return (Misplaced c)
            '\129001' -> return (Correct c)
            _         -> pfail
            
            -- windows does not allow copy pasting of unicodes into the powershell (not so powerful as it seems)
            {-
            'a'  -> return (Absent c)
            'm' -> return (Misplaced c)
            'c' -> return (Correct c)
            _         -> pfail
            -}
        where 
            -- can it be done without the lookahead?
            skipSpaces :: ReadPrec ()
            skipSpaces = do
                l <- look
                let n = length $ takeWhile (`elem` [' ','\t']) l
                traverse_ (const get) [1..n]
            

            void = (pure () <*)
            traverse_ f = void . traverse f
------------------------------
-- Functions                 |
------------------------------

-- | Given a guess and a target, returns the matches for each letter in guess.
match :: Guess -> Target -> [Match]
match G {getGuess=g} T {getTarget=t}  = snd $ match' AA.empty g t
    where 
        match' :: AA.AA Char () -> String -> String -> (AA.AA Char (), [Match])
        match' _     [] [] = (AA.empty, [])
        match' preds (g:gs) (t:ts) 
            | g == t     = f (Correct g :) res 
            | AA.member g preds || AA.member g post = f (Misplaced g :)  res
            | otherwise  = f (Absent g :) res  
            where
                res@(post,_) = match' (AA.insert t () preds) gs ts 
                f = bimap (AA.insert t ())
        match' _ _ _ = undefined 

-- | Checks if all the matches are correct.
fullMatch :: [Match] -> Bool
fullMatch = all isCorrect
    where
        isCorrect (Correct _) = True 
        isCorrect _           = False

-- | Projects the character out of a match.
getMatchChar :: Match -> Char 
getMatchChar (Absent c)    = c
getMatchChar (Misplaced c) = c
getMatchChar (Correct c)   = c