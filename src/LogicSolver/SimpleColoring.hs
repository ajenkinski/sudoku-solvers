module LogicSolver.SimpleColoring where

{- Implement simple coloring strategy, as described here:
- http://www.sudokuwiki.org/Singles_Chains
-}

import Control.Monad (guard)
import Data.List (intersect, nub)
import Data.Maybe (mapMaybe)
import LogicSolver.Utils
import Sudoku
import Text.PrettyPrint ((<+>))
import qualified Data.Graph as G
import qualified Data.Tree as T
import qualified Text.PrettyPrint as P


simpleColoringSimplifier :: Simplifier
simpleColoringSimplifier sudoku = findFirst tryChain (findChains sudoku)
  where tryChain chain@(Chain value assignments) =
          do let removals = findRemovals sudoku chain
             guard (not (null removals))
             addLog (P.text "Simple coloring: Removing" <+> doc value
                     <+> P.text "from" <+> doc removals
                     <+> P.text "based on connected chain" 
                     <+> doc [coord | (coord, _) <- assignments])
             return (removePossibleValues sudoku [(coord, [value]) | coord <- removals])


data Color = Black | White deriving (Eq, Show)

data Chain = Chain Value [(Coord, Color)] deriving (Show)

type Groups = [[Element]]


-- Returns a list of coordinates from which the chain value can be removed
findRemovals :: Sudoku -> Chain -> [Coord]
findRemovals sudoku (Chain value assignments) =
  -- Find pairs of squares in chain with different color assignments
  nub $ do (coord1, coord2) <- [(coord1, coord2) | [(coord1, color1), (coord2, color2)] <- ssolk 2 assignments,
                                color1 /= color2]
           -- Find squares connected to both coord1 and coord2
           [coord | (coord, Empty vals) <- connectedSquares sudoku coord1 `intersect` connectedSquares sudoku coord2,
            value `elem` vals] 


-- Return a list of colored chains for a sudoku board.
findChains :: Sudoku -> [Chain]
findChains sudoku = concatMap (chainsForValue (allGroupSquares sudoku)) [1..9]

-- Return a list of links between cells, where a link exists between two cells
-- if those are the only two cells in a group containing value as a possibility
findLinks :: Groups -> Value -> [(Coord, Coord)]
findLinks groups value = mapMaybe findLink groups
  where findLink group =
          case [coord | (coord, Empty vals) <- group, value `elem` vals] of
            [coord1, coord2] -> Just (coord1, coord2)
            _                -> Nothing

coordToVertex :: Coord -> G.Vertex
coordToVertex (row, col) = (row - 1) * 9 + (col - 1)

vertexToCoord :: G.Vertex -> Coord
vertexToCoord vertex = ((vertex `div` 9) + 1, (vertex `mod` 9) + 1)

chainsForValue :: Groups -> Value -> [Chain]
chainsForValue groups value =
  let links = findLinks groups value
      edges = [(coordToVertex c1, coordToVertex c2) | (c1, c2) <- links]
      graph = G.buildG (0, 80) edges
      -- Connected components consisting of more than one node correspond to chains
      comps = [comp | comp <- G.components graph, (not . null . T.subForest) comp]
      compToChain comp =
        -- Assign alternating color to each level
        let levelAssignments = zip (T.levels comp) (concat (repeat [Black, White]))
        in Chain value [(vertexToCoord v, color) | (vals, color) <- levelAssignments, v <- vals] 
   in [compToChain comp | comp <- comps]


