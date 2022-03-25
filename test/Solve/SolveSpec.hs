{-# LANGUAGE ScopedTypeVariables #-}
module Solve.SolveSpec where 

import Solve 
import Match (Match(..))
import qualified Match as M
import Text.Read
import AA (AA(..))
import qualified AA
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Control.Monad
import Prelude hiding (lookup)
import Data.Foldable (traverse_)
import Debug.Trace

newtype RandomMatch    = RM  {getRM :: Match} deriving Show

newtype RandomBlackMatch = RBM {getRBA :: ([Match],String)} deriving Show

newtype RandomYellowMatch = RYM {getRYA :: ([Match],String)} deriving Show

newtype LowerCase = LC {getLC :: String}

instance Arbitrary LowerCase where
    arbitrary = LC <$> listOf (elements ['a'..'z'])

instance Arbitrary RandomMatch where
    arbitrary = do
        c <- arbitrary `suchThat` (`elem` ['a'..'z'])
        let options = ($ c) <$> [Absent , Misplaced, Correct]
        RM <$> elements options

instance Arbitrary RandomBlackMatch where
    arbitrary = do
        let isBlack (Absent _) = True 
            isBlack _          = False
             
        ms <- fmap getRM <$> arbitrary `suchThat` \s -> any (isBlack . getRM) s

        let f s = fst <$> zip s ms

        LC s <- arbitrary `suchThat` (\(LC s) -> any (\c -> Absent c `elem`  ms) s && length s == length ms )

        pure $  RBM (ms,f s)

instance Arbitrary RandomYellowMatch where
    arbitrary = do
        let isYellow (Misplaced _) = True 
            isYellow _          = False
             
        ms <- fmap getRM <$> arbitrary `suchThat` \s -> any (isYellow . getRM) s

        let f s = fst <$> zip s ms

        LC s <- arbitrary `suchThat` (\(LC s) -> any (\(c,m) -> isYellow m && M.getMatchChar m == c)  (s `zip` ms) && length s == length ms )

        pure $  RYM (ms,f s)


sieveSpec :: Spec
sieveSpec = describe "sieve Spec" $ do
    prop "Sieving a string that matches any black character deletes it" $ 
        \(RBM (ms,s) :: RandomBlackMatch) -> case sieve ms [s] of
            [] -> True
            _  -> False
    prop "Sieving a string that matches any yellow character (in the same position!) deletes it" $ 
        \(RYM (ms,s) :: RandomYellowMatch) -> case sieve ms [s] of
            [] -> True
            _  -> False 
    it "watev" $ null $ sieve [Misplaced 'o', Absent 'c', Absent 'e', Correct 'a', Absent 'n'] ["cupid"]

internalsSpecSolve :: IO ()
internalsSpecSolve = traverse_ hspec [sieveSpec]