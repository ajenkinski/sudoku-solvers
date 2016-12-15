module LogicSolver.SimpleColoring (simpleColoringSimplifier) where

{- Implement simple coloring strategy, as described here:
- http://www.sudokuwiki.org/Singles_Chains
-}

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (intersect, nub)
import Data.Maybe (mapMaybe, listToMaybe)
import LogicSolver.Utils
import Sudoku
import Text.PrettyPrint ((<+>))
import qualified Data.Graph as G
import qualified Data.Tree as T
import qualified Text.PrettyPrint as P


simpleColoringSimplifier :: Simplifier
simpleColoringSimplifier sudoku = findFirst tryChain (findChains sudoku)
  where trySet chain@(Chain value assignments) =
          do let squaresToSet = findTwiceInGroupSet sudoku chain
             guard (not (null squaresToSet))
             addLog (P.text "Simple coloring: Setting squares" <+> doc squaresToSet
                     <+> P.text "to" <+> doc value <+> P.text "based on connected chain"
                     <+> doc [coord | (coord, _) <- assignments])
             return (foldl (\sud coord -> assignValue sud (coord, value)) sudoku squaresToSet)
        tryRemoval chain@(Chain value assignments) =
          do let removals = findTwoColorRemovals sudoku chain
             guard (not (null removals))
             addLog (P.text "Simple coloring: Removing" <+> doc value
                     <+> P.text "from" <+> doc removals
                     <+> P.text "based on connected chain" 
                     <+> doc [coord | (coord, _) <- assignments])
             return (removePossibleValues sudoku [(coord, [value]) | coord <- removals])
        tryChain chain = trySet chain <|> tryRemoval chain


data Color = Black | White deriving (Eq, Show)

data Chain = Chain Value [(Coord, Color)] deriving (Show)

type Groups = [[Element]]


{- 
Returns a list of coordinates from which the chain value can be removed as a possibility.
This function implements the rule that if two squares A and B in the chain are opposite
colors, then one of them must be the chain value, so the chain value can't be in any of 
the other squares connected to both A and B.
-}
findTwoColorRemovals :: Sudoku -> Chain -> [Coord]
findTwoColorRemovals sudoku (Chain value assignments) =
  -- Find pairs of squares in chain with different color assignments
  nub $ do (coord1, coord2) <- [(coord1, coord2) | [(coord1, color1), (coord2, color2)] <- ssolk 2 assignments,
                                color1 /= color2]
           -- Find squares connected to both coord1 and coord2 which contain value as a possibility
           [coord | (coord, Empty vals) <- connectedSquares sudoku coord1 `intersect` connectedSquares sudoku coord2,
            value `elem` vals] 

isConnected :: Sudoku -> Coord -> Coord -> Bool
isConnected sudoku coord1 coord2 = coord1 `elem` [coord | (coord, _) <- connectedSquares sudoku coord2]

findTwiceInGroupSet :: Sudoku -> Chain -> [Coord]
findTwiceInGroupSet sudoku (Chain _ assignments) =
  let offColor = listToMaybe [color1 | [(coord1, color1), (coord2, color2)] <- ssolk 2 assignments,
                              color1 == color2 && isConnected sudoku coord1 coord2]
  in maybe [] (\color -> [coord | (coord, color') <- assignments, color /= color']) offColor


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


