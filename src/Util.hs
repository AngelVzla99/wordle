{-|
Module      : Util
Description : Provides various utilities that shall be used throughout the program.
License     : GPL-3
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util where 

import System.IO
    ( IO,
      FilePath,
      putStr,
      readFile,
      hGetBuffering,
      hSetBuffering,
      hSetEcho,
      BufferMode(NoBuffering),
      stdin,
      stdout,
      hFlush )  
import Control.Monad ( Monad(return, (>>)), Functor(fmap), MonadPlus(mzero) )
import Prelude hiding (lookup)
import AA ( AA, fromList )
import Foreign.C.Types ( CInt(..) )
import Data.Char ( chr )
import Control.Exception (catch, SomeException, throw)

-- | Number of turns in the game
turns :: Int
turns = 6

-- | Path of the file where are saved the words
dictionary :: String
dictionary = "american-english"

---------------------------------
-- Functions                    |
---------------------------------

-- | Checks whether a string is a valid 5 letter word.
validWord :: String -> Bool
-- ... but with style... applicative style.
validWord  = (&&) <$> ((==5) . length ) <*> all isLetter
    where
        isLetter :: Char -> Bool
        isLetter = (`elem` ['a' .. 'z'])

-- | Filters in all the valid words from a list.
fiveLetterWords :: [String] -> [String]
fiveLetterWords = filter validWord 

-- | Loads a dictionary represented as an AA tree.
loadDictionary :: FilePath -> IO (AA.AA String String)
loadDictionary path = do
    contents <- readFile path
    let lista = fiveLetterWords $ words contents
        diag x = (x,x)
    return $ fromList $ fmap diag lista

-- | persistently asks for a yes or no, printing the given message each time.
yesOrNo :: String -> IO Bool
yesOrNo text = withNoBuffer $ do
    putStr' ( text ++ " (y/n) ?" )
    c <- getChar' 
    case c of 
        'y' -> putStr' " y\n" >>return True
        'n' -> putStr' " n\n" >>return False 
        _   -> putStr' "\n"   >> yesOrNo text



-----------------------------------------------------------
-- No need to read past here ------------------------------
-----------------------------------------------------------

-- | Perform an IO action with no buffering (stdin AND stdout) and no echo.
-- restoring the states after it's done (NOTE: if an exception happens, it's NOT guaranteed that the
-- restoration is done! primarely because i'm not sure what happens if the catch isinterrupted by another 
-- exception...)
withNoBuffer :: IO a -> IO a
withNoBuffer ma = do 
    stdinBuffer  <- hGetBuffering stdin 
    stdoutBuffer <- hGetBuffering stdout
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdout False

    -- try catch goes brrrrrrrr
    a <- ma `catch` (\(e :: SomeException) -> 
        hSetBuffering stdin stdinBuffer 
        >> hSetBuffering stdout stdoutBuffer 
        >> hSetEcho stdout True 
        >> throw e ) 

    hSetBuffering stdin stdinBuffer
    hSetBuffering stdout stdoutBuffer
    hSetEcho stdout True
    
    pure a

-- | Prints a string to the `stdout` ASAP. Needed if buffer mode is anything other than `NoBuffering`.
-- (windows is not very nais to us :()
putStr' :: String -> IO ()
putStr' s = putStr s >> hFlush stdout

-- | Same
putChar' :: Char -> IO ()
putChar' c = putChar c >> hFlush stdout

-- Multi-platform version of `getChar` which has a fix for a GHC bug with Windows cmd/Powershell
getChar' :: IO Char
getChar' = withNoBuffer $ do  -- just in case, DON'T echo
#ifdef mingw32_HOST_OS
      -- Windows has to do things...
      c <- c_getch
      let c' = chr . fromEnum $ c
      return c'
#else
    -- Linux, Unix, Mac OS X can just use the normal getChar
    getChar
#endif

#ifdef mingw32_HOST_OS  
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
#endif


