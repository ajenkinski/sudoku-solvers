module LogicSolver.MedusaColoring where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Control.Monad.Writer (lift)
import qualified Data.Graph.Inductive.Graph as GR
import qualified Data.Graph.Inductive.Query.BFS as BFS
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           LogicSolver.Utils
import           Sudoku
import           Text.PrettyPrint ((<+>), (<>))
import qualified Text.PrettyPrint as P

-- Use 3D-medusa strategy (http://sudopedia.enjoysudoku.com/3D_Medusa.html)
medusaSimplifier :: Simplifier
medusaSimplifier board = 
  let chains = findMedusaChains board
      trySimplifier f = findFirst (`f` board) chains
  in findFirst trySimplifier [medusaOneCellContradictionSimplifier
                             ,medusaOneGroupContradictionSimplifier
                             ,medusaOneCellContradictionSimplifier
                             ,medusaOneCellEliminationSimplifier
                             ,medusaOneGroupEliminationSimplifier
                             ,medusaIntersectionEliminationSimplifier]


{-
Implement 3D Medusa coloring, as described here:
http://sudopedia.enjoysudoku.com/3D_Medusa.html

For 3D Medusa coloring, we think of the sudoku puzzle as a 3D
cube, with each digit's plane as a layer in the cube.  So we have
a 9x9x9 cube: one layer for each of the 9 candidates.  When there
are only 2 candidates in a line along any dimension, those 2 cells
in the cube are connected.  Additionally when there are only two
candidates in a box in one candidate's plane, they are connected.

Converting this idea to a graph, the graph nodes are [coord value]
pairs, meaning a value is a candidate at coord, and there is an
edge between two nodes if either a) they form a conjugate pair, or
b) they form a bivalue cell, i.e. a cell with only 2
candidates. Once this graph is constructed, we can apply connected
components and bivalue coloring algorithms to it just as in simple
coloring, to find bi-colored chains.
-}

{- Find all conjugate pairs in a board.  A conjugate pair is a pair of
 cells in a group which are the only two cells in that group which
 contain a particular possibility. -}
findConjugatePairs :: Sudoku -> [((Coord, Coord), Value)]
findConjugatePairs board = [((coord1, coord2), val) | ((_, val), [coord1, coord2]) <- Map.assocs groupVals]
  where groupValCoord = 
          do (coord@ (row, col), Empty vals) <- emptySquares board
             group                           <- [Row row, Col col, blockOfCoord coord]
             val                             <- vals
             return ((group, val), [coord])
        
        groupVals :: Map.Map (Group, Value) [Coord]
        groupVals = Map.fromListWith (++) groupValCoord

-- Find all bivalue cells
findBivalueCells :: Sudoku -> [Element]
findBivalueCells board = [e | e@(_, Empty [_, _]) <- emptySquares board]

data Color = Black | White deriving (Eq, Show)
type Label = (Coord, Value)
type MedusaChain = Map.Map Label Color

otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black

isUncolored :: Label -> MedusaChain -> Bool
isUncolored label chain = not $ Map.member label chain

findMedusaChains :: Sudoku -> [MedusaChain]
findMedusaChains board = map componentToChain components
  where edges = 
          [((coord1, val), (coord2, val)) |
           ((coord1, coord2), val) <- findConjugatePairs board]
           ++ 
          [((coord, val1), (coord, val2)) |
           (coord, Empty [val1, val2]) <- findBivalueCells board]

        (graph, _) = graphFromEdges edges

        components = DFS.components graph
        
        colors = [Black, White]
        
        componentToChain comp = 
          let levels = BFS.level (head comp) graph
          in Map.fromList [(fromJust $ GR.lab graph v, colors !! (level `mod` 2)) | (v, level) <- levels]


assignToChainColor :: MedusaChain -> Color -> String -> Simplifier
assignToChainColor chain onColor description board =
  do let toAssign = Map.keys (Map.filter (onColor ==) chain)
     addLog (P.text "3D Medusa: Found" <+> P.text description <> P.text ". Making assignments" 
             <+> doc toAssign)
     return $ foldl assignValue board toAssign

groupByKey :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupByKey f xs = [(f (head group), group) | group <- List.groupBy (\x y -> f x == f y) xs]

{-
Looks for a contradiction in a medusa chain, where two candidates in a
single cell have the same color.  This identifies that as the 'off'
color in the chain, thereby allowing assigning all the 'on' colored
cells.
-}
medusaOneCellContradictionSimplifier :: MedusaChain -> Simplifier
medusaOneCellContradictionSimplifier chain board = findFirst tryCoord byCoord
  where byCoord = groupByKey (fst . fst) (Map.assocs chain)
        
        tryCoord (coord, assignments) =
          do offColor <- findFirst (\(color, group) -> do guard (length group > 1)
                                                          return color) 
                                   (groupByKey snd assignments)
             assignToChainColor chain (otherColor offColor) 
                                      ("single cell contradiction in " ++ show coord) board


{-
Looks for a contradiction in a medusa chain, where two candidates for
a single digit in a single group have the same color. This identifies
that as the 'off' color in the chain, thereby allowing assigning all
the 'on' colored cells.
-}
medusaOneGroupContradictionSimplifier :: MedusaChain -> Simplifier
medusaOneGroupContradictionSimplifier chain board = findFirst tryPair (ssolk 2 (Map.assocs chain))
  where tryPair [((coord1, val1), color1), ((coord2, val2), color2)] =
          do guard (arePeers coord1 coord2 && val1 == val2 && color1 == color2)
             assignToChainColor chain (otherColor color1)
                                ("single group contradiction for value " ++ show val1 
                                 ++ " in cells " ++ show coord1 ++ " and " ++ show coord2)
                                board
        tryPair _ = error "Unexpected input"

{- 
Looks for 2 colors in a single cell, which means uncolored candidates
in that cell can be removed.
-}
medusaOneCellEliminationSimplifier :: MedusaChain -> Simplifier
medusaOneCellEliminationSimplifier chain board = findFirst tryCoord byCoord
  where byCoord = groupByKey (fst . fst) (Map.assocs chain)
        tryCoord (coord, assignments) = 
          do guard (length (List.nub (map snd assignments)) > 1)
             candidates <- lift $ possibleValues (get board coord)
             let uncolored = [c | c <- candidates, isUncolored (coord, c) chain]
             guard (not $ null uncolored)
             addLog (P.text "3D Medusa: 2 colors in cell" <+> doc coord 
                     <+> P.text " allow eliminating candidates" <+> doc uncolored)
             return $ removePossibleValues board [(coord, uncolored)]

{- 
Looks for 2 colors for one digit in one group with more than 2
candidates for that digit.  The uncolored candidates can be
eliminated.
-}
medusaOneGroupEliminationSimplifier :: MedusaChain -> Simplifier
medusaOneGroupEliminationSimplifier chain board = findFirst tryPair (ssolk 2 (Map.assocs chain))
  where tryPair [((coord1, val1), color1), ((coord2, val2), color2)] = 
          do guard (val1 == val2 && color1 /= color2 && arePeers coord1 coord2)
             let uncolored = [coord | (coord, Empty vals) <- peersOfCoords board coord1 coord2,
                                      val1 `elem` vals,
                                      isUncolored (coord, val1) chain]
             guard (not $ null uncolored)
             return $ removePossibleValues board [(coord, [val1]) | coord <- uncolored]
        tryPair _ = error "Unexpected input"

{-
An uncolored candidate can see a cell where the same digit is colored,
while its own cell has another candidate with the opposite color. The
uncolored candidate can be eliminated.
-}
medusaIntersectionEliminationSimplifier :: MedusaChain -> Simplifier
medusaIntersectionEliminationSimplifier chain board = findFirst tryPair (ssolk 2 (Map.assocs chain))
  where tryPair [((coord1, val1), color1), ((coord2, val2), color2)] = 
          do guard (arePeers coord1 coord2 && color1 /= color2 && val1 /= val2)
             (coord, val) <- lift (isCandidate coord1 val2 <|> isCandidate coord2 val1)
             
             addLog (P.text "3D Medusa: Intersection Elimination: Removing" <+> doc val
                    <+> P.text "from" <+> doc coord <+> P.text "due to intersection between"
                    <+> doc (coord1, val1) <+> P.text "and" <+> doc (coord2, val2))
             return $ removePossibleValues board [(coord, [val])]
        tryPair _ = error "Unexpected input"

        isCandidate coord val = 
          do vals <- possibleValues (get board coord)
             guard (val `elem` vals && isUncolored (coord, val) chain)
             return (coord, val)

