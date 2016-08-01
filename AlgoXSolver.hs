{-
A Sudoku solver which uses Knuth's Algorithm X to solve a sudoku
puzzle.  Sudoku is an example of an "exact cover" problem, or more
specifically, an "exact hitting set" problem.  Algorithm X is an
algorithm for solving exact cover problems.  See
http://en.wikipedia.org/wiki/Exact_cover for more information.

Algorithm X represents the exact cover problem as a matrix of 0s and
1s.  I wanted to try a naive, purely functional implementation of
Algorithm X, rather than Knuth's Dancing Links algorithm.  Since it
has more zeros than 1s, I'll represent it as a sparse matrix.  So the
first order of business is to define a sparse matrix datatype.
-}

import qualified Data.Map as M
import Data.List (minimumBy)
import qualified Data.Ix as Ix
import qualified Data.Char as Char
import System.Console.GetOpt
import System.Environment (getArgs)
import Control.Monad (foldM, when)
import qualified System.IO as SIO
import qualified Control.Parallel.Strategies as PS
import Data.Array (listArray, (!))

{-
This program doesn't actually use the Sudoku module, except to
pretty-print a board.
-}

import qualified Sudoku

{-
************** Sparse Matrix Datatype ***************

The sparse matrix datatype only needs to support the operations
required by the Algorithm X implementation.  It's a boolean matrix.
It needs to support the following operations:

1) get column numbers of all non-zero elements in a row,

2) get row numbers of all non-zero elements in a column,

3) delete a row,

4) delete a column,

5) test if matrix is empty, i.e. has 0 rows or 0 columns.

The representation I've chosen supports these operations quite
efficiently, with the assumption that the matrix is very sparse.  This
is true for sudoku; the matrix representation of the sudoku problem is
a 729x324 matrix, but it has only 4 non-zero values in each row, and 9
non-zero values in each column.

-}

type Coord = (Int,Int)

{-
A sparse matrix is represented by two maps, one for rows and one for
columns.  Each row in the row map contains a set of the non-zero
columns in that row, and each column in the column map contains a set
of the non-zero rows in that column.
-}

data Sparse = SP { rows, cols :: M.Map Int [Int] }

{-
Create a Sparse from a list of (row,col) coordinates of the non-zero
elements.  The first two params are the number of rows and columns in
the matrix.  This is needed because there needs to be an entry even
for empty rows and columns initially, and if only the coords list were
passed in, then rows or columns with no non-zeros would not be
included at all in the matrix.  Row and column numbering starts at 0.
-}

fromList :: Int -> Int -> [Coord] -> Sparse
fromList nRows nCols coords = SP rows cols
    where rows = foldl insertInRow initRows coords
          cols = foldl insertInCol initCols coords
          initRows = M.fromAscList [(r, []) | r <- [0..nRows-1]]
          initCols = M.fromAscList [(c, []) | c <- [0..nCols-1]]
          insertInRow m (r,c) = M.adjust (\s -> c:s) r m
          insertInCol m (r,c) = M.adjust (\s -> r:s) c m

isEmpty :: Sparse -> Bool
isEmpty (SP rows cols) = M.null rows || M.null cols

-- Return a list of the columns in row which are set

getRow :: Int -> Sparse -> [Int]
getRow r (SP rows cols) 
    | Just row <- M.lookup r rows = filter (`M.member` cols) row
    | otherwise = []

-- Return a list of the rows in column which are set

getCol :: Int -> Sparse -> [Int]
getCol c (SP rows cols)
    | Just col <- M.lookup c cols = filter (`M.member` rows) col
    | otherwise = []

-- Return a list of all the column numbers of the matrix.

getCols :: Sparse -> [Int]
getCols = M.keys . cols

-- Delete a row from a sparse

delRow :: Int -> Sparse -> Sparse
delRow r (SP rows cols) = SP (M.delete r rows) cols

-- Delete a column from a sparse

delCol :: Int -> Sparse -> Sparse
delCol c (SP rows cols) = SP rows (M.delete c cols)


{-
************* Algorithm X ********************

Takes as input a Sparse which represents an exact cover problem, and
returns a list of all solutions, where a solution is a list of row
numbers in the solution.  Returns an empty list if it didn't find any
solutions.

-}

algoX :: Sparse -> [[Int]]
algoX sp = algoX' sp []
    where algoX' sp ans =
              -- Uses the list monad to implement backtracking search
              if isEmpty sp then [ans]
              else do colNum <- chooseCol sp
                      rowNum <- getCol colNum sp
                      let sp' = trim (getRow rowNum sp) sp
                      algoX' sp' (rowNum:ans)
          chooseCol sp =
              -- Choose column with the least 1s
              let lens = [(c, length (getCol c sp)) | c <- getCols sp]
                  (col,len) = minimumBy (\(_,l1) (_,l2) -> compare l1 l2) lens
              in if len == 0 then [] else [col]
          trim row sp =
              -- remove columns set in row, and any other rows which
              -- have a column which is set in row
              let sp' = foldl (\s c -> delCol c s) sp row
              in foldl (\s r -> delRow r s) sp' (concat (map (`getCol` sp) row))


{-
************ Sudoku *************

This section contains the code to create a sparse matrix
representation of an "exact cover problem" representation of a sudoku
puzzle.  The "exact cover" page on wikipedia describes this
representation.

Let P be the set of possibilties.  Each element of P is a
(row,col,value) tuple, where row and col refer to a row and column on
a sudoku board, and value is the value assigned to that cell.  R1C1#1
represents the possibility of assigning the value 1 to the cell at row
1, col 1.  There are a total of 9x9x9 = 729 possibilites in P.  In the
matrix representation of the problem, each possibility will correspond
to a row, so the matrix has 729 rows.

The columns in the matrix will each represent a constraint set.  We'll
define 4 kinds of constraints:

* Row-Column: Each intersection of a row and column, i.e, each cell,
  must contain exactly one number.

* Row-Number: Each row must contain each number exactly once

* Column-Number: Each column must contain each number exactly once.

* Box-Number: Each box must contain each number exactly once.

It so happens that there are 81 of each kind of constraint, for a
total of 81+81+81+81=324 constraint sets.  So the matrix has 324
columns.

Each constraint has a constraint set corresponding to it, which is the
set of possibilities which could satisfy the constraint.

* Row-Column: A Row-Column constraint set contains all the
  possibilities for a particular row,column, i.e. cell. For example,
  the constraint set for the cell R1C1 would be:
  
  R1C1 = {R1C1#1,R1C1#2,R1C1#3,...,R1C1#9}

  There are 81 Row-Column constraint sets, one for each cell.

* Row-Number: A Row-Number constraint set contains all the
  possibilities for a particular row,number.  I.e. it lists all the
  columns which a particular number could appear in in a row.  For
  example:

  R1#1 = {R1C1#1,R1C2#1,R1C3#1,...,R1C9#1}

  There are 9 rows, and 9 values, for a total of 81 row-number
  constraint sets.

* Column-Number: A Column-Number constraint set contains all the
  possibilities for a particular column,number pair.  For example:

  C1#1 = {R1C1#1,R2C1#1,R3C1#1,...,R9C1#1}

* Box-Number: A Box-Number constraint set contains all the
  possibilities for a particular box,number pair. This refers to the
  3x3 boxes.  Assume the boxes are numbered 1 through 9 in row-major
  order.  Example:

  B1#1 = {R1C1#1,R1C2#1,R1C3#1,R2C1#1,R2C2#1,...,R3C3#1}

The matrix is 729x324.  It has one row for each of the 729
possibilities in P, and one column for each of the 324 constraint sets
described above.  Each cell of the matrix is True if the possibility
for that row is contained in the constraint set for that column.  So
for example, the intersection of the row for R1C1#1, and the column
for C1#1 would be true, because the C1#1 constraint set contains R1C1#1.


I need to decide how the possibilties in P map to row numbers, and how
the constraint sets map to column numbers.  Rows and columns will be
numbered starting at 0.  

For rows, each possibility can be represented by a (row,col,number)
tuple; they'll be ordered as (1,1,1), (1,1,2),
....,(1,2,1),(1,2,2),...,(9,9,9), i.e. the same order that

  Ix.range ((1,1,1),(9,9,9))

would return.

For columns, the first 81 columns will be the Row-Column constraints,
the second 81 will be the Row-Number constraints, the third 81 will be
the Column-Number constraints, and the fourth 81 will be the
Box-Number constraints.  Each of these will be numbered from (1,1) to
(9,9) as Ix.range would return.

If algoX finds a solution, it will contain 81 rows, corresponding to
the 81 possibilities.  Then all I need to do is convert the row
numbers back to possibilities, and I have my sudoku solution.

The possibilityToRow and rowToPossibility functions convert between
possibilities and matrix row numbers.

-}

-- A possibility is a (row, col, value) tuple, representing the possibility of value being in row,col
type Possibility = (Int, Int, Int)

-- A (row, col) tuple
type Constraint = (Int, Int)

possibilityToRow :: Possibility -> Int
possibilityToRow = Ix.index ((1,1,1),(9,9,9))

-- Convert a row number to a possibility

rowToPossibility :: Int -> Possibility
rowToPossibility = (possibilities !)
    where 
      range = ((1,1,1),(9,9,9))
      possibilities = listArray (0, (Ix.rangeSize range) - 1) (Ix.range range)

-- Convert a constraint, such as R1C1, to a column number, starting at 0
-- for (1,1).

constraintToCol :: Constraint -> Int
constraintToCol = Ix.index ((1,1),(9,9))


-- The inverse of constraintToCol

colToConstraint :: Int -> Constraint
colToConstraint c = (c `div` 9 + 1, c `mod` 9 + 1)

{-
The solve function takes as input a string representation of a sudoku
puzzle, and returns a list of solutions, each being represented by a
list of 81 (row,col,number) tuples.
-}

solve :: String -> [[(Int,Int,Int)]]
solve str =
    let initBoard = zip (Ix.range ((1,1),(9,9))) (map Char.digitToInt str)
        assigned = M.fromList (filter ((/= 0).snd) initBoard)
        cellNumbers (r,c)
            | Just n <- M.lookup (r,c) assigned = [n]
            | otherwise = [1..9]
        possibilities = [(r,c,n) |
                         r <- [1..9], c <- [1..9], n <- cellNumbers (r,c)]
        rowColCoords =
            -- The Row-Column constraints.  I don't include
            -- possibilities which conflict with the initial
            -- assignments; this is how the initial assignments
            -- become part of the constraints.
            [(possibilityToRow (r,c,n), constraintToCol (r,c)) | 
             (r,c,n) <- possibilities]
        rowNumCoords =
            -- The Row-Number constraints
            [(possibilityToRow (r,c,n), constraintToCol (r,n) + 81) |
             (r,c,n) <- possibilities]
        colNumCoords =
            -- The Column-Number constraints
            [(possibilityToRow (r,c,n), constraintToCol (c,n) + (81*2)) |
             (r,c,n) <- possibilities]
        boxNum r c =
            -- convert a (r,c) to a box number 1 through 9
            let br = (r-1) `div` 3
                bc = (c-1) `div` 3
            in br * 3 + bc + 1
        boxNumCoords =
            -- The Box-Number constraints
            [(possibilityToRow (r,c,n),
              constraintToCol (boxNum r c, n) + (81*3)) |
             (r,c,n) <- possibilities]
        sparse = fromList 729 324 (concat [rowColCoords, rowNumCoords,
                                   colNumCoords, boxNumCoords])
    in do solution <- algoX sparse
          return $ map rowToPossibility solution

{- ************ Main function and cmdline option parsing *********** -}

-- cmdline option parsing
type Opt = (String,String)

options :: [OptDescr Opt]
options =
    [ Option ['f'] ["file"] (ReqArg ((,) "file") "FILE")
                 "File containing sudoku boards"
    , Option ['P'] ["pfile"] (ReqArg ((,) "pfile") "FILE")
                 "File containing sudoku boards, will solve in parallel"
    , Option ['p'] ["puzzle"] (ReqArg ((,) "puzzle") "PUZZLE")
                 "Puzzle string as 81 digits 0-9"
    ]

usage = usageInfo "Usage: AlgoXSolver [OPTION]" options

parseOpts :: [String] -> [(String,String)]
parseOpts args =
  case getOpt Permute options args of
    (opts, _, []) -> opts
    (_, _, errs) -> error (concat errs ++ usage)

doPuzzle :: String -> IO ()
doPuzzle puzzle
    | (solution:_) <- solve puzzle
    = putStrLn $ show (Sudoku.mkSudoku [((r,c), Sudoku.Assigned n) |
                                        (r,c,n) <- solution])
    | otherwise = putStrLn "Couldn't solve it"

{-
Note that in doFile, the "when (seq n False)..." statement in the
foldM function exists to force n to be strictly evaluated.  If I don't
do this, none of the puzzles would actually be solved until the
subsequent putStrLn, so the dots would all be printed immediately.
-}

doFile :: FilePath -> IO ()
doFile file = do
  contents <- readFile file
  let puzzles = words contents
      solutions = map solve puzzles
  -- print a '.' every 100 puzzles so I know it's making progress
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  (numSolved,total) <- foldM (\(n,t) s -> 
                                  do when (seq n False) (error "Force n eval")
                                     when (t `mod` 100 == 0) (putStr ".")
                                     return (if null s then n else n + 1, t + 1))
                       (0,0) solutions
  putStrLn ("\nSolved " ++ show numSolved ++ " puzzles out of " ++
            show total)

-- An evaluation strategy for a list, which evaluates 100 element
-- chunks of the list in parallel

strategy = PS.parListChunk 100 PS.rdeepseq

-- Solve all puzzles in file in parallel.  Pass +RTS -Nx -RTS to the
-- runtime, where x is the number of concurrent threads you'd like to
-- use.
doParFile :: FilePath -> IO ()
doParFile file = do
  contents <- readFile file
  let puzzles = words contents
      solutions = (map ((take 1) . solve) puzzles) `PS.using` strategy 
      numSolved = sum (map length solutions)
      numPuzzles = length puzzles
  putStrLn ("Solved " ++ show numSolved ++ " puzzles out of " ++ show numPuzzles)

doMain opts 
    | Just file <- lookup "file" opts = doFile file
    | Just file <- lookup "pfile" opts = doParFile file
    | Just puzzle <- lookup "puzzle" opts = doPuzzle puzzle
    | otherwise = error usage

main = do
  args <- getArgs
  doMain (parseOpts args)
