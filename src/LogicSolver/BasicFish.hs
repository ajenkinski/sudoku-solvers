module LogicSolver.BasicFish (
                              simpleXWingSimplifier
                              , simpleSwordfishSimplifier
                              , simpleJellyfishSimplifier
) where

{- Implement basic fish strategies, as described here:
- http://sudopedia.enjoysudoku.com/Fish.html
- Basic fish is a generalization of the simple X-Wing strategy to any number of
- rows or columns n, where n >= 2.  If n rows each contain at most n squares with
- possibility X, in the same n columns, then X can be removed from any other
- squares in those n columns.  The same applies to rows.
-}

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import LogicSolver.Utils
import Sudoku
import Text.PrettyPrint ((<+>))
import qualified Data.IntMap as IM
import qualified Data.List as List
import qualified Text.PrettyPrint as P


simpleXWingSimplifier     = simpleFishSimplifier "X-Wing" 2
simpleSwordfishSimplifier = simpleFishSimplifier "Swordfish" 3
simpleJellyfishSimplifier = simpleFishSimplifier "Jellyfish" 4

simpleFishSimplifier :: String -> Int -> Simplifier
simpleFishSimplifier ruleName n sudoku =
  solveForGroupType Row Col snd ruleName n sudoku <|> solveForGroupType Col Row fst ruleName n sudoku

-- Row or Col
type GroupType = Int -> Group

between :: Ord a => a -> a -> a -> Bool
between a b x = x >= a && x <= b

solveForGroupType :: GroupType -> GroupType -> (Coord -> Int) -> String -> Int -> Simplifier
solveForGroupType groupType otherGroupType otherComponent ruleName n sudoku =
  let groups = map groupType [1..9]
      valMaps = filter (not . IM.null . snd) [(g, makeValMap g) | g <- groups]
  in findFirst trySetOfGroups (ssolk n valMaps)
  where makeValMap group = 
          let m = IM.fromListWith (++) [(v, [c]) | (c, Empty vs) <- groupSquares sudoku group, v <- vs] 
          in IM.filter (between 2 n . length) m
        trySetOfGroups set = findFirst (tryValue set) (IM.keys (snd (head set)))
        tryValue set val = do
          let coordss = mapMaybe (IM.lookup val . snd) set
              coords = List.concat coordss
              -- The column (or row) numbers containing possibility val
              matches = List.nub (map otherComponent coords)
          guard (length coordss == n)
          guard (length matches == n)

          -- Now we know we've found a fish formation.  Check whether any possibilities can be removed
          let coordsToRemove = [c | group <- map otherGroupType matches, 
                                    (c, Empty vs) <- groupSquares sudoku group, 
                                    val `elem` vs && c `notElem` coords]
          guard (not (null coordsToRemove))

          addLog (P.text "Found" <+> P.text ruleName <+> P.text "in" <+> doc (map fst set)
                  <+> P.text ", in" <+> doc coords <+> P.text "." <+> P.text "Deleting" <+> doc val
                  <+> P.text "from" <+> doc coordsToRemove)
          return (removePossibleValues sudoku [(c, [val]) | c <- coordsToRemove])

