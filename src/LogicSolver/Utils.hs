module LogicSolver.Utils where

import           Control.Applicative (Alternative)
import           Control.Monad.Writer (WriterT, tell)
import           Data.Foldable (asum)
import           Data.List (tails)
import           Sudoku
import qualified Text.PrettyPrint as P

-- convert any showable object to a P.Doc
doc :: Show a => a -> P.Doc
doc = P.text . show

{- 
  Finds the first element of a list for which the given function returns 
  a non-empty value.

  Note: What this function actually does is fold a list of Alternatives with the <|>
  operator.  So the behavior of this function actually depends on how <|>  is
  implemented for the type being passed in.  For Maybe, <|> chooses the first
  argument which isn't Nothing.  However for lists, <|> = ++, so if 'f' is [], 
  then this function would just concatenate all the lists rather than return the 
  first non-empty list.
-}
findFirst :: Alternative f => (a -> f b) -> [a] -> f b
findFirst f xs = asum (map f xs)

{-| A simplifier is a function which takes a Sudoku board, and tries
to eliminate some of the possible values from the empty squares'
possible-value lists.  It returns the transformed Sudoku puzzle, or
Nothing if it couldn't eliminate any possible values.  Simplifiers
also take in a P.Doc which contains descriptions of all the
simplifications done to the board so far, and return a new doc with a
description of any new simplifications appended.  -}

type LogWriter a = WriterT [P.Doc] Maybe a
type SimplifiedSudoku = LogWriter Sudoku
type Simplifier = Sudoku -> SimplifiedSudoku

addLog :: P.Doc -> LogWriter ()
addLog doc = tell [doc]

-- Return all "subsets of length k" of xs
ssolk :: Int -> [a] -> [[a]]
ssolk k xs
    | k == 0    = [[]]
    | otherwise =
        [x:ss | (x:rest) <- tails xs, ss <- ssolk (k-1) rest]
