module LogicSolver.YWing (yWingSimplifier) where

{- Implement the Y-Wing strategy, as described here:
- http://www.sudokuwiki.org/Y_Wing_Strategy
-}

import Control.Monad (guard)
import Data.List (intersect)
import LogicSolver.Utils
import Sudoku
import Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as P


yWingSimplifier :: Simplifier
yWingSimplifier sudoku = findFirst tryWingPair yWings
  where -- all empty squares with exactly two possibilities
        potentialPivots :: [Element]
        potentialPivots = [square | square@(_, Empty [_, _]) <- allSquares sudoku]

        -- All y-wings, including ones which won't result in any simplifications
        yWings :: [(Element, (Coord, Coord, Value))]
        yWings = [(pivot, wingPair) | pivot <- potentialPivots, wingPair <- findWingPairs sudoku pivot]

        -- Given a y-wing, check if it results in a simplification
        tryWingPair :: (Element, (Coord, Coord, Value)) -> LogWriter Sudoku
        tryWingPair ((pCoord, _), (coord1, coord2, valToRemove)) = do
          let intersection = connectedSquares sudoku coord1 `intersect` connectedSquares sudoku coord2
              candidates = [coord | (coord, Empty vs) <- intersection, valToRemove `elem` vs]
          guard (not (null candidates))
          addLog (P.text "Y-Wing with pivot" <+> doc pCoord <+> P.text "and wings" <+> doc coord1 
                  <+> P.text "and" <+> doc coord2 <+> P.text "allows removing" <+> doc valToRemove 
                  <+> P.text "from " <+> doc candidates)
          return (removePossibleValues sudoku [(coord, [valToRemove]) | coord <- candidates])


{-| Returns a list of all y-wing pairs for the given pivot element, along with the value shared by each
- y-wing
-}
findWingPairs :: Sudoku -> Element -> [(Coord, Coord, Value)]
findWingPairs sudoku (pivotCoord, Empty vs@[a, b]) = do
  let potentialWings = [(coord, (x, y)) | (coord, Empty [x, y]) <- connectedSquares sudoku pivotCoord,
                        length ([x, y] `intersect` vs) == 1]
  [(coord1, (x1, y1)), (coord2, (x2, y2))] <- ssolk 2 potentialWings
  case isYWing (a, b) (x1, y1) (x2, y2) of
    Just c -> return (coord1, coord2, c)
    _      -> []
findWingPairs _ _ = error "Argument is not a potential pivot"

{-| 
Signature: isYWing pivot wing1 wing2

Check if a set of three squares could form a Y-Wing. Assumes that we've already
verified that the squares each are empty with two possibilities, and that each 
wing shares one possibility with the pivot.  Input is the three pairs of 
possibilities.  The squares form a y-wing if each of the pivot's two possibilities
is shared by one wing, and the wings share a third possibility.  Returns the 
value shared by the two wings if this is a y-wing.
-}

isYWing :: (Value, Value) -> (Value, Value) -> (Value, Value) -> Maybe Value
isYWing (a, b) (x1, y1) (x2, y2) =
  let pivotw1 = [a, b] `intersect` [x1, y1]
      pivotw2 = [a, b] `intersect` [x2, y2]
      wc      = [x1, y1] `intersect` [x2, y2]
  in case (pivotw1, pivotw2, wc) of
       ([a'], [b'], [c]) -> if a' /= b' then Just c else Nothing
       _                 -> Nothing

