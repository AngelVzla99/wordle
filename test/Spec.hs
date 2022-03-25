import  AA.AASpec  
import Match.MatchSpec
import Solve.SolveSpec
import Data.Foldable (sequenceA_)

main :: IO ()
main = sequenceA_
    [ internalsSpecMatch
    , internalsSpecAA
    , internalsSpecSolve
    ]
