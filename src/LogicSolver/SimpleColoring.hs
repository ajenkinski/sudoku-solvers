module LogicSolver.SimpleColoring (
  simpleColoringSimplifier
  , multiColoringSimplifier)
where

{- Implement simple coloring strategy, as described here:
- http://www.sudokuwiki.org/Singles_Chains
-}

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import qualified Data.Graph.Inductive.Graph as GR
import qualified Data.Graph.Inductive.PatriciaTree as PT
import qualified Data.Graph.Inductive.Query.BFS as BFS
import qualified Data.Graph.Inductive.Query.DFS as DFS
import           Data.List (intersect, nub, find)
import           Data.Maybe (mapMaybe, listToMaybe, fromJust)
import           LogicSolver.Utils
import           Sudoku
import           Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as P

simpleColoringSimplifier :: Simplifier
simpleColoringSimplifier sudoku = findFirst tryChain (findChains sudoku)
  where trySet chain@(Chain { chainValue = value, chainAssignments = assignments}) =
          do let squaresToSet = findTwiceInGroupSet sudoku chain
             guard (not (null squaresToSet))
             addLog (P.text "Simple coloring: Setting squares" <+> doc squaresToSet
                     <+> P.text "to" <+> doc value <+> P.text "based on connected chain"
                     <+> doc [coord | (coord, _) <- assignments])
             return (foldl (\sud coord -> assignValue sud (coord, value)) sudoku squaresToSet)
        tryRemoval chain@(Chain { chainValue = value, chainAssignments = assignments}) =
          do let removals = findTwoColorRemovals sudoku chain
             guard (not (null removals))
             addLog (P.text "Simple coloring: Removing" <+> doc value
                     <+> P.text "from" <+> doc removals
                     <+> P.text "based on connected chain" 
                     <+> doc [coord | (coord, _) <- assignments])
             return (removePossibleValues sudoku [(coord, [value]) | coord <- removals])
        tryChain chain = trySet chain <|> tryRemoval chain


data Color = Black | White deriving (Eq, Show)

data Chain = Chain { chainValue :: Value,
                     chainAssignments :: [(Coord, Color)],
                     neighbors :: Coord -> [Coord]
                   }

type Groups = [[Element]]


{- 
Returns a list of coordinates from which the chain value can be removed as a possibility.
This function implements the rule that if two squares A and B in the chain are opposite
colors, then one of them must be the chain value, so the chain value can't be in any of 
the other squares connected to both A and B.
-}
findTwoColorRemovals :: Sudoku -> Chain -> [Coord]
findTwoColorRemovals sudoku (Chain { chainValue = value, chainAssignments = assignments}) =
  -- Find pairs of squares in chain with different color assignments
  nub $ do (coord1, coord2) <- [(coord1, coord2) | [(coord1, color1), (coord2, color2)] <- ssolk 2 assignments,
                                color1 /= color2]
           -- Find squares connected to both coord1 and coord2 which contain value as a possibility
           [coord | (coord, Empty vals) <- connectedSquares sudoku coord1 `intersect` connectedSquares sudoku coord2,
            value `elem` vals] 

isConnected :: Sudoku -> Coord -> Coord -> Bool
isConnected sudoku coord1 coord2 = coord1 `elem` [coord | (coord, _) <- connectedSquares sudoku coord2]

findTwiceInGroupSet :: Sudoku -> Chain -> [Coord]
findTwiceInGroupSet sudoku (Chain { chainAssignments = assignments}) =
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

coordToVertex :: Coord -> GR.Node
coordToVertex (row, col) = (row - 1) * 9 + (col - 1)

vertexToCoord :: GR.Node -> Coord
vertexToCoord vertex = ((vertex `div` 9) + 1, (vertex `mod` 9) + 1)

chainsForValue :: Groups -> Value -> [Chain]
chainsForValue groups value =
  let links = findLinks groups value
      edges = [(coordToVertex c1, coordToVertex c2) | (c1, c2) <- links]
      nodes = concat [[a, b] | (a, b) <- edges]
      graph = GR.mkUGraph nodes edges :: PT.Gr () ()
      -- Connected components correspond to chains
      comps = DFS.components graph
      compToChain comp =
        -- Assign alternating color to each level
        let levels = BFS.level (head comp) graph
            colors = [Black, White]
            levelAssignments = [(vertexToCoord v, colors !! (level `mod` 2)) |
                                (v, level) <- levels]
            compGraph = GR.subgraph comp graph
            neighborsFn coord = map vertexToCoord (GR.neighbors compGraph (coordToVertex coord))
        in Chain value levelAssignments neighborsFn
   in [compToChain comp | comp <- comps]

multiColoringForValue :: Groups -> Value -> Simplifier
multiColoringForValue groups value sudoku = findFirst id simplifications
  where
    chains = chainsForValue groups value
    
    coord2Chain :: Coord -> Maybe Chain
    coord2Chain coord =
      find (\(Chain { chainAssignments = assigns}) ->
              (any (\(coord', _) -> coord == coord') assigns)) chains

    simplifications :: [SimplifiedSudoku]
    simplifications = do
      [chain1, chain2] <- ssolk 2 chains
      (chain1Coord, _) <- chainAssignments chain1
      (chain2Coord, _) <- chainAssignments chain2
      guard (arePeers chain1Coord chain2Coord)

      -- Now we know chain1Coord and chain2Coord are weakly connected
      chain1Neighbor <- neighbors chain1 chain1Coord
      chain2Neighbor <- neighbors chain2 chain2Coord

      -- Now we can eliminate value from cells that can see both
      -- chain1Neighbor and chain2Neighbor
      let eliminations = [coord |
                          (coord, squ) <- peersOfCoords sudoku chain1Neighbor chain2Neighbor,
                          hasCandidate squ value]
      guard (not (null eliminations))

      return $ doSimplification eliminations

    doSimplification :: [Coord] -> SimplifiedSudoku
    doSimplification eliminations =
      -- There are two possiblities if an elimination is found.  If the
      -- elimination cell is part of a chain, then we know that that's
      -- the off color of that chain, and can set all the 'on' cells of
      -- that chain.  Otherwise we just perform the eliminations found.
      let chainCoords = mapMaybe (\coord -> do chain <- coord2Chain coord
                                               return (chain, coord)) eliminations
      in if null chainCoords then doEliminations eliminations else doAssignments chainCoords

    doEliminations :: [Coord] -> SimplifiedSudoku
    doEliminations eliminations = do
      -- Eliminations don't fall on a chain, so just eliminate possibilities
      addLog (P.text "Multi Coloring allowed eliminating" <+> doc value
              <+> P.text "from" <+> doc eliminations)
      return (removePossibleValues sudoku [(coord, [value]) | coord <- eliminations])

    doAssignments :: [(Chain, Coord)] -> SimplifiedSudoku
    doAssignments chainCoords = do
      let coordsToSet = nub $ concatMap coordsToSetForChain chainCoords
      addLog (P.text "Multi Coloring allowed setting cells" <+> doc coordsToSet
              <+> P.text "to" <+> doc value)
      return (foldl (\s coord -> assignValue s (coord, value)) sudoku coordsToSet)

    coordsToSetForChain :: (Chain, Coord) -> [Coord]
    coordsToSetForChain (Chain { chainAssignments = assignments }, chainCoord) =
      let (_, offColor) = fromJust (find ((== chainCoord) . fst) assignments)
      in [coord | (coord, color) <- assignments, color /= offColor]
                       
      
    
multiColoringSimplifier :: Simplifier
multiColoringSimplifier sudoku =
  let groups = allGroupSquares sudoku
  in findFirst (\value -> multiColoringForValue groups value sudoku) [1..9]
  
