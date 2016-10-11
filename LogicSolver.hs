-- A program to solve sudoku puzzles

module Main ( main
            , forcedMoveSimplifier
            , pinnedSquareSimplifier
            , hiddenSetSimplifier
            , nakedSetSimplifier
            , intersectionRemovalSimplifier
            , simpleXWingSimplifier
            , blockOfCoord
            , isInGroup
            , solve
            , parseOpts
) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad (foldM, when, guard)
import Control.Monad.Writer (WriterT, tell, runWriterT, lift)
import Data.Foldable (asum)
import Data.List (tails, partition, (\\), union, groupBy, sortBy)
import Data.Maybe (mapMaybe, listToMaybe, fromJust, isJust)
import Sudoku
import System.Console.GetOpt
import System.Environment (getArgs)
import Text.PrettyPrint ((<>), (<+>), vcat)
import qualified Control.Parallel.Strategies as PS
import qualified Data.IntMap as IM
import qualified System.IO as SIO
import qualified Text.PrettyPrint as P

-- convert any showable object to a P.Doc
doc :: Show a => a -> P.Doc
doc = P.text . show


{- 
  Finds the first element of a list for which the given function returns 
  a non-empty value.

  Note: What this function actually does fold a list of Alternatives with the <|>
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
type Simplifier = Sudoku -> LogWriter Sudoku

addLog :: P.Doc -> LogWriter ()
addLog doc = tell [doc]

-- | A forced move is an unassigned cell with only one value in its
-- possibilities list, so it's forced to have that value.  This
-- simplifier makes the first forced move it finds.

forcedMoveSimplifier :: Simplifier
forcedMoveSimplifier s =
    do (c, v) <- lift (listToMaybe [(c,v) | (c, Empty [v]) <- emptySquares s])
       addLog (P.text "Forced move:" <+> doc c <+> P.text "=" <+> doc v)
       return (assignValue s (c,v))


-- | If only a single square in a group can contain a particular
-- value, then that square is called a "pinned" square.  For example,
-- if in row 1, the only square which has the value 2 as a possibility
-- is column 3, then square (1,3) must have the value 2.  This
-- simplifier finds the first pinned square and assigns the pinned
-- value to it.

pinnedSquareSimplifier :: Simplifier
pinnedSquareSimplifier s = findFirst tryGroup allGroups
  where
    tryGroup group = findFirst tryValue unassigned
        where
          unassigned = [1..9] \\ [v | (_,Assigned v) <- squares]
          tryValue v =
              case [c | (c, Empty vs) <- squares, v `elem` vs] of
                [c] -> do addLog (describe group (c,v))
                          return (assignValue s (c,v))
                _ -> empty
          squares = groupSquares s group
    describe group (c,v) =
        P.text "Pinned square: in" <+> doc group
        <> P.text ", only cell" <+> doc c
        <+> P.text "can contain" <+> doc v

------------------

-- Return all "subsets of length k" of xs
ssolk :: Int -> [a] -> [[a]]
ssolk k xs
    | k == 0    = [[]]
    | otherwise =
        [x:ss | (x:rest) <- tails xs, ss <- ssolk (k-1) rest]

-- Stolen from Simon Peyton Jones' sudoku solver:
-- If N keys collectively map to a set of exactly N values
-- AND any of those N values are mapped to by some other keys,
-- THEN return (N-keys, N-vals, del-items)
-- Where del-items are the (key,[val]) that are in the input set,
-- but are not part of the N-keys, N-vals group; these are the ones to delete
--
-- The incoming [val] are assumed distinct
findTuple :: (Eq key, Ord val)
             => Int -> [(key, [val])]
             -> Maybe ([key], [val], [(key,val)])
findTuple n all_items = findFirst trySubset (ssolk n all_items)
    where
      trySubset items
          | length vals == n && not (null del_items)
              = Just (keys, vals, del_items)
          | otherwise = Nothing
          where
            keys = map fst items
            vals = foldl1 union (map snd items)
            del_items = [(k,v) |
                         (k,vs) <- all_items, k `notElem`keys,
                         v <- vs, v `elem`vals]

cmpFst :: Ord a => (a, b) -> (a, c) -> Ordering
cmpFst (a,_) (b,_) = compare a b

eqFst :: Eq a => (a, b) -> (a, c) -> Bool
eqFst (a,_) (b,_) = a == b

groupByFst :: Ord a => [(a,b)] -> [(a,[b])]
groupByFst pairs = [(a, b : map snd rest) | (a,b):rest <- groups]
    where groups = groupBy eqFst (sortBy cmpFst pairs)


-- | hiddenSetSimplifier implements the rule that if n cells in a
-- group (row, column, or block) collectively contain n possible
-- values, and none of the other cells in that group contain any of
-- the values in the tuple, then any values other than the tuple
-- values can be removed from the possible values lists of the cells
-- containing the tuple.  An example may make this clearer.  If, in a
-- row, two unassigned cells contain the values (3,4) in their
-- possible values list, and no other cells in that row contain either
-- 3 or 4 in their possible values list, then those two cells must
-- contain the values 3 and 4, so any other values can be removed from
-- those two cells' possible values lists.

hiddenSetSimplifier :: Simplifier
hiddenSetSimplifier s = findFirst tryN [2..8]
    where
      tryN n = findFirst (tryGroup n) allGroups
      tryGroup n group =
          do (tuple, coords, del_vals) <- lift (findTuple n val_coords)
             let to_remove = groupByFst [(c, v)  | (v, c) <- del_vals]
             addLog (describe group tuple coords to_remove)
             return (removePossibleValues s to_remove)
          where val_coords =
                    groupByFst [(v,c) | (c, Empty vs) <- squares, v <- vs]
                squares = groupSquares s group
      describe group tuple coords to_remove =
          P.text "Hidden set" <+> doc tuple
                <+> P.text "in cells" <+> doc coords
                <+> P.text "in" <+> doc group
                <> P.text ".  Deleting" <+> doc to_remove

-- | Like hiddenSetSimplifier, but implements the rule that a set
-- of N cells collectively contains exactly N values, then no other
-- cell in that group can contain any of those N values.

nakedSetSimplifier :: Simplifier
nakedSetSimplifier s = findFirst tryN [2..8]
    where
      tryN n = findFirst (tryGroup n) allGroups
      tryGroup n group =
          do (coords, tuple, del_coords) <- lift (findTuple n empties)
             let to_remove = groupByFst del_coords
             addLog (describe group coords tuple to_remove)
             return (removePossibleValues s to_remove)
          where empties = [(c,vs) | (c, Empty vs) <- groupSquares s group]
      describe group coords tuple to_remove =
          P.text "Naked set" <+> doc tuple
                <+> P.text "in cells" <+> doc coords
                <+> P.text "in" <+> doc group
                <> P.text ".  Deleting" <+> doc to_remove

-- some utility functions

-- | Return the block that coord is in
blockOfCoord :: Coord -> Group
blockOfCoord (r, c) = Block (((r-1) `div` 3)*3 + 1, ((c-1) `div` 3)*3 + 1)

-- | Return true if a Coord is in a group
isInGroup                 :: Coord -> Group -> Bool
isInGroup (r, _) (Row r') = r == r'
isInGroup (_, c) (Col c') = c == c'
isInGroup coord block     = blockOfCoord coord == block


-- | If the squares in a row or column that have a particular
-- possibility only appear in a single block, then that possibility
-- must occur in the 3-square intersection of the row/column and the
-- block, and thus cannot appear in the other 6 squares of the block.
-- Similarly, if the squares in block that have a particular
-- possibility only appear in a single row or column, then that value
-- can be removed from any other squares in that row or column.

intersectionRemovalSimplifier :: Simplifier
intersectionRemovalSimplifier s = findFirst tryGroup rowsAndCols
    where
      rowsAndCols = [Row r | r <- [1..9]] ++ [Col c | c <- [1..9]]

      tryGroup g = findFirst (tryValue g) (groupPossibleValues g)

      tryValue       :: Group -> Value -> LogWriter Sudoku
      tryValue g val =
          let blocks = intersectingBlocks g
          in findFirst (\(g1, g2) -> tryIntersection g1 g2 val)
                 ([(g, b) | b <- blocks] ++ [(b, g) | b <- blocks])

      -- Return a list of all possible values in a group
      groupPossibleValues   :: Group -> [Value]
      groupPossibleValues g = foldl union [] [vs | (_, Empty vs) <- groupSquares s g]

      intersectingBlocks         :: Group -> [Group]
      intersectingBlocks (Row r)   = [blockOfCoord (r, 1), blockOfCoord (r, 4), blockOfCoord (r, 7)]
      intersectingBlocks (Col c)   = [blockOfCoord (1, c), blockOfCoord (4, c), blockOfCoord (7, c)]
      intersectingBlocks (Block _) = error "Oops"

      -- Return a list of the coordinates in group of squares which have val as a possible
      groupValSquares           :: Group -> Value -> [Coord]
      groupValSquares group val = [c | (c, Empty vs) <- groupSquares s group, val `elem` vs]

      -- If any square in group can contain val, and the only squares in group that can contain
      -- val are the squares which intersect with otherGroup, then remove val as a possible
      -- from any squares in otherGroup which don't intersect with group
      tryIntersection                      :: Group -> Group -> Value -> LogWriter Sudoku
      tryIntersection group otherGroup val =
          let groupCoords = groupValSquares group val
              otherGroupCoords = groupValSquares otherGroup val
              coordsToRemove = otherGroupCoords \\ groupCoords
          in do guard (not (null groupCoords) && null (groupCoords \\ otherGroupCoords) && not (null coordsToRemove))
                addLog (describe group otherGroup coordsToRemove val)
                return (removePossibleValues s [(crd, [val]) | crd <- coordsToRemove])

      describe group otherGroup otherSquares val =
          P.text "In" <+> doc group <> P.text "," <+> doc val
            <+> P.text "can only be in" <+> doc otherGroup <> P.text "."
            <+> P.text "Deleting" <+> doc val <+> P.text "from"
            <+> doc otherSquares

-- | Simple X-Wing: To find a simple X-Wing, look for a row in the
-- puzzle that has only two squares that can contain a particular
-- number (also known as a “conjugate pair”), then find another row
-- that has only two squares that can contain that number. If the
-- squares line up into the same two columns, forming a box shape,
-- then you’ve found an X-Wing, and the number can be removed from all
-- the other squares in the two columns.  The same can be applied to
-- columns as well.  Also, this can be generalized to more than two.
-- For instance if you can find three rows that each contain a
-- particular value in only the same three columns, then you can
-- remove that value from other squares in those columns.

simpleXWingSimplifier :: Simplifier
simpleXWingSimplifier s = findFirst tryPairOfGroups groupPairs
    where
      groupPairs = ssolk 2 (map Row [1..9]) ++ ssolk 2 (map Col [1..9])
      tryPairOfGroups [g1, g2] =
          let g1Empties = filter (isEmpty.snd) (groupSquares s g1)
              g2Empties = filter (isEmpty.snd) (groupSquares s g2)
              g1ValMap = IM.fromListWith (++) [(v, [c]) | (c, sq) <- g1Empties,
                                               v <- fromJust (possibleValues sq)]
              g2ValMap = IM.fromListWith (++) [(v, [c]) | (c, sq) <- g2Empties,
                                               v <- fromJust (possibleValues sq)]
          in findFirst (tryValue g2ValMap) (IM.toList g1ValMap)
          where
            tryValue g2ValMap (val, g1Coords) =
                do g2Coords <- lift (IM.lookup val g2ValMap)
                   guard (length g1Coords == 2 && length g2Coords == 2)
                   guard (coordsMatch g1 g1Coords g2Coords)
                   -- Found a XWing pattern.  Now see if there are any possibles to remove
                   coordsToRemove <- lift (findCoordsToRemove val g1 g2 g1Coords)
                   addLog (describe g1 g2 g1Coords g2Coords val coordsToRemove)
                   return (removePossibleValues s [(c, [val]) | c <- coordsToRemove])
      tryPairOfGroups _ = error "Call with length 2 lists only"

      coordsMatch (Row _) g1Coords g2Coords = map snd g1Coords == map snd g2Coords
      coordsMatch (Col _) g1Coords g2Coords = map fst g1Coords == map fst g2Coords
      coordsMatch (Block _) _ _             = error "Oops, called with block"

      findCoordsToRemove val (Row r1) (Row r2) [(_, c1), (_, c2)] =
          -- see if any squares in column c1 or c2, except at rows r1 or r2, contain val
          let squares = groupSquares s (Col c1) ++ groupSquares s (Col c2)
          in case mapMaybe getCoordIfContainsVal squares of
               [] -> Nothing
               coords -> Just coords
          where
            getCoordIfContainsVal ((r,c), sq) =
                do possibles <- possibleValues sq
                   guard (r /= r1 && r /= r2 && elem val possibles)
                   return (r,c)
      findCoordsToRemove val (Col c1) (Col c2) [(r1, _), (r2, _)] =
          -- see if any squares in row r1 or r2, except at columns c1 or c2, contain val
          let squares = groupSquares s (Row r1) ++ groupSquares s (Row r2)
          in case mapMaybe getCoordIfContainsVal squares of
               [] -> Nothing
               coords -> Just coords
          where
            getCoordIfContainsVal ((r,c), sq) =
                do possibles <- possibleValues sq
                   guard (c /= c1 && c /= c2 && elem val possibles)
                   return (r,c)
      findCoordsToRemove _ _ _ _ = error "Called with unexpected args"

      describe group1 group2 g1Coords g2Coords valToRemove coordsToRemove =
          P.text "Found X-Wing in" <+> doc group1 <+> P.text "and" <+> doc group2
            <+> P.text ", in" <+> doc g1Coords <+> P.text "and" <+> doc g2Coords <> P.text "."
            <+> P.text "Deleting" <+> doc valToRemove <+> P.text "from" <+> doc coordsToRemove


-- | Unique Rectangles: This one is hard enough to explain that I'll
-- just refer to the Suduku Susser.pdf document in this directory,
-- starting on page 49.  Basically, this takes advantage of the fact
-- that every valid sudoku has a single unique solution.  All the
-- unique rectangle variations look for a possible deadly pattern, and
-- take steps to make sure it can't exist.
--
-- A deadly pattern is a set of 4 squares, in 2 rows, 2 columns, and 2
-- blocks, which each contain only the same 2 possible values.  If you
-- ever get to this point while solving a puzzle, you know you've made
-- a mistake, because if this were valid, it would mean there were
-- more than one solution possible for the puzzle.  For example, if
-- the squares R1C1, R1C2, R4C1, and R4C2 all contained the
-- possibilities (2,3), then both (R1C1=2, R1C2=3, R4C1=3, R4C2=2) and
-- (R1C1=3, R1C2=2, R4C1=2, R4C2=3) would be possible solutions which
-- satisfied all the constraints.


-- A helper function for the unique rectangle simplifiers.  Finds all
-- possible deadly patterns, i.e. sets of 4 squares in 2 rows, 2
-- columns, and 2 blocks, where all 4 squares share 2 possibilities,
-- and 2 of the 4 squares share only those 2 possibilities.

--findPossibleDeadlyPatterns :: Sudoku -> [((Element, Element), (Element, Element))]
--findPossibleDeadlyPatterns s =

{- With this set of simplifiers:

forcedMoveSimplifier,
pinnedSquareSimplifier,
hiddenSetSimplifier,
nakedSetSimplifier,
intersectionRemovalSimplifier

this solves 31017 out of the 36628 min sudoku puzzles, and takes 2:58
running in parallel on my MacBook Pro.  Removing
intersectionRemovalSimplifier makes it solve only 25916 puzzles, but
then it takes only 1:41.  So nearly half the time is being spent in
that one simplifier, but it's quite effective.

7/15/2010 - Added simpleXWingSimplifier.  Now it solves 31153 puzzles in 3:17.
4/30/2011 - Improved intersectionRemovalSimplifier.  Now it solves 31319 puzzles.
-}

simplifiers = [ forcedMoveSimplifier
              , pinnedSquareSimplifier
              , nakedSetSimplifier
              , hiddenSetSimplifier
              , intersectionRemovalSimplifier
              , simpleXWingSimplifier
              ]

isSolution :: Sudoku -> Bool
isSolution s = all (isAssigned.snd) (allSquares s)

-- | Solves a sudoku puzzle as much as it can, and returns the result.
-- The result may not be completely solved if the solver couldn't
-- proceed any further; use isSolution to test whether the result is
-- completely solved.
--
-- The solve function works by applying each of the simplifier
-- functions in simplifiers to the board, until one of them succeeds
-- in simplifying something.  In that case, it recurses with the
-- simplified board as the argument, so the simplifiers are all run
-- again in sequence until one of them succeeds.  The recursion ends
-- when none of the simplifiers succeed in making any changes.  In
-- that case, either the puzzle is solved, or the solver failed to
-- solve it.  The second output is a Doc which contains a text
-- description of the steps used to solve the puzzle.

solve :: Sudoku -> (Sudoku, P.Doc)
solve s = let (s', docs) = fromJust (runWriterT (solve' s)) in (s', vcat docs)
  where solve' s = (findFirst ($ s) simplifiers >>= solve') <|> return s


difficultSample =
    "001000600" ++
    "000701000" ++
    "650000091" ++
    "002403900" ++
    "930000086" ++
    "008206700" ++
    "890000042" ++
    "000304000" ++
    "005000100"

-- cmdline option parsing
type Opt = (String,String)

options :: [OptDescr Opt]
options =
    [Option ['f'] ["file"] (ReqArg ((,) "file") "FILE")
                "File containing sudoku boards",
     Option ['P'] ["pfile"] (ReqArg ((,) "pfile") "FILE")
                "File containing sudoku boards, will solve in parallel. Pass +RTS -Nx -RTS to specify num threads",
     Option ['p'] ["puzzle"] (ReqArg ((,) "puzzle") "PUZZLE")
                "Puzzle string as 81 digits 0-9",
     Option ['v'] ["verbose"] (NoArg ("verbose", "-v"))
                "Verbose output",
     Option ['u'] ["unsolved"] (NoArg ("unsolved", "-u"))
                "Output unsolved puzzles to stdout"
    ]

parseOpts :: [String] -> [(String,String)]
parseOpts args =
  case getOpt Permute options args of
    (opts, _, []) -> opts
    (_, _, errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: SudokuMain [OPTION]"

doPuzzle :: String -> IO ()
doPuzzle puzzle =
    let s = parseSudoku puzzle
        (s', doc) = solve s
    in do putStrLn $ verboseShow s
          putStrLn $ P.render doc
          putStrLn (if isSolution s' then show s' else verboseShow s')

doFile :: FilePath -> IO ()
doFile file = do
  contents <- readFile file
  let puzzleStrings = words contents
      puzzles = map parseSudoku puzzleStrings
      solutions = map (fst.solve) puzzles
  -- print a '.' every 100 puzzles so I know it's making progress
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  -- The call to seq in the foldM function is necessary to make puzzles be solved
  -- as the loop goes along, otherwise all the dots get printed immediately, and
  -- the puzzles don't actually get solved until numSolved gets printed out.
  (numSolved,total) <- foldM (\(n,t) s ->
                                  do when (seq n False) (error "Force eval")
                                     when (t `mod` 100 == 0) (putStr ".")
                                     return (if isSolution s then n+1 else n,
                                             t + 1))
                       (0,0) solutions
  putStrLn ("Solved " ++ show numSolved ++ " puzzles out of " ++ show total)

-- An evaluation strategy for a list, which evaluates N element
-- chunks of the list in parallel

strategy :: PS.Strategy [Sudoku]
strategy = PS.parListChunk 200 PS.rdeepseq

-- Solve all puzzles in file in parallel.  Pass +RTS -Nx -RTS to the
-- runtime, where x is the number of concurrent threads you'd like to
-- use.
doParFile :: FilePath -> Bool -> IO ()
doParFile file outputUnsolved = do
  contents <- readFile file
  let puzzleStrings = words contents
      puzzles = map parseSudoku puzzleStrings
      solutions = map (fst.solve) puzzles `PS.using` strategy
      (solved, unsolved) = partition (isSolution.fst) (zip solutions puzzleStrings)
      numSolved = length solved
      numPuzzles = length puzzles
  if outputUnsolved then
    mapM_ (putStrLn.snd) unsolved
  else
    putStrLn ("Solved " ++ show numSolved ++ " puzzles out of " ++ show numPuzzles)

doMain opts
    | Just file <- lookup "file" opts = doFile file
    | Just file <- lookup "pfile" opts = doParFile file (isJust (lookup "unsolved" opts))
    | Just puzzle <- lookup "puzzle" opts = doPuzzle puzzle
    | otherwise = doPuzzle difficultSample

main = do
  args <- getArgs
  doMain (parseOpts args)
