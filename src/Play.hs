{-|
Module      : Play
Description : Provides the main logic of the game.
License     : GPL-3
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve
Stability   : experimental
Portability : POSIX
-}
module Play where 
-- Aca falta un export de initialState al programa principal

import Util
import System.Random (randomRIO)
import Match -- No deberian ser necesarios 
import AA    
import System.IO 
import Control.Monad
import Data.Char

---------------------------------
-- Types                        |
---------------------------------

-- | Represents the current game state.
data GameState = GS 
    { played :: Int                  -- ^ Total games played
    , won    :: Int                  -- ^ Total games won
    , streak :: Int                  -- ^ Current (win) streak
    , target :: Target               -- ^ Target word.
    , dict   :: AA.AA String String  -- ^ dictionary used for picking the words.
    }

-- | Represents the result of a game.
data Result = Win Target | Lose Target

---------------------------------
-- Instances                    |
---------------------------------

instance Show GameState where
    show GS {played=_played, won=_won, streak=_streak} 
        =  "Played: "  ++ show _played 
        ++ " Won: "    ++ show _won 
        ++ " Lost: "   ++ show (_played - _won) 
        ++ " Streak: " ++ show _streak

instance Show Result where
    show (Win (T t))  = "Got it! It was \171" ++ t ++ "\187 \128526 "
    show (Lose (T t)) = "Bummer! It was \171" ++ t ++ "\187 \128128 "

---------------------------------
-- Auxiliar Functions           |
---------------------------------

takeAA :: GameState ->  AA String String
takeAA (GS p w s t d) = d 

---------------------------------
-- Functions                    |
---------------------------------

-- | Initial State of the Game.
initialState :: IO GameState 
initialState = GS 0 0 0 (T "") <$> loadDictionary dictionary

-- | Plays the game time and time again updating the state each round.
playTheGame :: GameState -> IO ()
playTheGame state = do
    newTarget <- pickTarget (dict state)
    res       <- play (changeTarget state newTarget) 
    let newState = updState state res

    print newState
    print newTarget
    continue <- yesOrNo "Another round"

    if continue 
        then playTheGame newState 
        else putStrLn "End of the game"
    
    where
        updState :: GameState -> Result -> GameState
        updState (GS p w s t d) (Win _) = GS (p+1) (w+1) (s+1) (T "") d
        updState (GS p w s t d) (Lose _) = GS (p+1) w 0 (T "") d
        changeTarget :: GameState -> Target -> GameState
        changeTarget (GS p w s t d) t2 = GS p w s t2 d

-- PARA PROBAR TEMPORALMENTE, LUEGO BORRAR
menu = do
    --hSetBuffering stdout NoBuffering
    --hSetBuffering stdin NoBuffering
    state <- initialState
    playTheGame state

-- | Plays a single round.
play :: GameState -> IO Result
play state = play' state 1 turns

-- | Aux function that actually plays the round.
play' :: GameState -> Int -> Int -> IO Result
play' gs current max =  do
    putStr' $ "Guess " ++ show current ++ "? "
    word <- readFive 

    let t = target gs
        d = dict gs 
        m = match (G word) t
        printMatch = putChar' ' ' >> (putStr' . show)  m >> putChar' '\n'

    case (word `AA.member` d, fullMatch m) of
        (False,_) -> putStrLn (" Your guess: '" ++ word ++ "' is not a valid word!") >> play' gs current max
        (_,True)  -> printMatch >> pure (Win t)
        (_,False) -> printMatch >> if current + 1 == max then pure $ Lose t else play' gs (current +1) max

-- | Persistintly reads 5 valid characters. 
readFive :: IO String
readFive =  reverse <$> readFive' 5 5 ""

-- | Persistintly reads `max` valid characters.
readFive' :: Int -> Int -> String -> IO String
readFive' n max acc = do
    c <- toLower <$>  getChar'

    case (c, c `elem` ['a'..'z']) of 
        ('\b',_) -> when (n < max) eraseChar >> readFive' (min max (n+1)) max (drop 1 acc)
        ('\n',_) -> if n == 0 then pure acc else readFive'  n max acc 
        ('\r',_) -> if n == 0 then pure acc else readFive'  n max acc 
        (_,True) -> if n > 0 then putChar' c >> readFive' (n-1) max (c:acc) else readFive' n max acc 
        _        -> readFive' n max acc 

-- | Erases a character from the `stdout`
eraseChar :: IO ()
eraseChar = putChar' '\b' >> putChar' ' ' >> putChar' '\b'

-- | Erases `n` character from the `stdout`
eraseNChar :: Int -> IO ()
eraseNChar = flip replicateM_ (eraseChar >> putChar' ' ') >=> const (putChar' '\b')

-- | Randomly picks a target.
pickTarget :: AA String String -> IO Target
pickTarget arbol = T . fst <$> pick arbol 

-- | Pick a target in a uniform way.
pick :: AA String String -> IO (String,Int)
pick = foldM union ([],0) 
    where 
        union (strPrev,len) str = do 
            val <- randomRIO (0,len)
            if val==0 
                then return (str,len+1)
                else return (strPrev,len+1)


