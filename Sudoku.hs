{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Sudoku (
               Value
              , Square(..), Group(..)
              , verboseShow
              , isEmpty, isAssigned
              , possibleValues, assignedValue
              , Coord
              , Element
              , Sudoku
              , get
              , parseSudoku, mkSudoku
              , groupSquares, allGroups, allGroupSquares
              , connectedSquares
              , emptySquares, allSquares
              , assignValue, removePossibleValues
) where

import qualified Data.Array as A
import Data.Array ((//), (!))
import qualified Data.Char as Char
import Data.List ((\\), delete, intersperse)
import qualified Control.Parallel.Strategies as PS
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

---- EXPORTED FUNCTIONS ----

-- The type of symbol used to fill in board elements
type Value = Int

-- A square can be empty or assigned a value.  Empty squares contain
-- a list of possible values.

data Square = Empty [Value] | Assigned Value deriving (Show,Eq,Generic,NFData)

isEmpty, isAssigned :: Square -> Bool

isEmpty (Empty _) = True
isEmpty _         = False

isAssigned (Assigned _) = True
isAssigned _            = False

possibleValues              :: Square -> Maybe [Value]
possibleValues (Empty vs)   = Just vs
possibleValues (Assigned _) = Nothing

assignedValue              :: Square -> Maybe Value
assignedValue (Assigned v) = Just v
assignedValue (Empty _)    = Nothing

type Coord = (Int, Int)
type Element = (Coord, Square)

-- Sudoku board representation.  The board field is the main board
-- representation.  It's a 2D array of Squares.  The rows, cols, and
-- blocks fields are just there to cache different subsets of the
-- board squares, so that the groupSquares function executes quickly.
-- I found this to give almost a 2x speedup when running my solver,
-- because the solver algorithms call these functions many times for
-- each board.

data Sudoku = Sudoku {board :: A.Array Coord Square,
                      rows, cols, blocks :: A.Array Int [Element]
                     }
              deriving (Generic,NFData)

get :: Sudoku -> Coord -> Square
get s c = (board s)!c

-- A datatype to specify groups of cells on a board.

data Group = Row Int | Col Int | Block Coord deriving (Show,Eq)

-- Constructs a Sudoku board from a list of Values, which represent
-- the values of the board cells in row major order, with 0s for
-- unassigned cells.
initialSudoku    :: [Value] -> Sudoku
initialSudoku es =
    let init = A.listArray ((1,1),(9,9)) (map valToSquare es)
    in addAnnotations $ mkSudokuFromArray init
    where
      valToSquare v = if v == 0 then (Empty []) else (Assigned v)

      -- Find all valid assignments for a single element
      findValidValues :: Sudoku -> Coord -> [Value]
      findValidValues s c =
          case (board s)!c of
            Assigned _ -> error "Can't find possible values for assigned square"
            otherwise ->
                [1..9] \\ [v | (_, Assigned v) <- connectedSquares s c]

      -- Annotate all unassigned elements with possible values
      addAnnotations :: Sudoku -> Sudoku
      addAnnotations s = mkSudoku (map annotate (A.assocs (board s)))
          where annotate a@(_, Assigned _) = a
                annotate (c, _) = (c, Empty $ findValidValues s c)

-- Displays a sudoku board in a way which displays assigned values,
-- and empty squares for unassigned values.
instance Show Sudoku where
    show sud = foldl1 (.) (map showRow [1..9]) rowSeparator
        where rowSeparator = "+---+---+---+---+---+---+---+---+---+\n"
              showRow r    =
                  showString rowSeparator . showChar '|' .
                  foldl1 (.) (map (showSquare.snd) (groupSquares sud (Row r))) .
                  showChar '\n'
              showSquare (Assigned v) = showChar ' ' . shows v . showString " |"
              showSquare (Empty _) = showString "   |"

-- Displays a more verbose board description, which not only shows
-- assigned values, but also shows the possibleValues lists associated
-- with unassigned cells.
verboseShow :: Sudoku -> String
verboseShow sud = foldl1 (.) (map showRow [1..9]) $ rowSeparator
    where rowSeparator = concat (replicate 9 "+-------") ++ "+\n"
          showRow r =
              let row = map snd (groupSquares sud (Row r))
              in showString rowSeparator . showChar '|' .
                 foldl1 (.) (map (showSquare 0) row) . showString "\n|" .
                 foldl1 (.) (map (showSquare 1) row) . showString "\n|" .
                 foldl1 (.) (map (showSquare 2) row) . showChar '\n'
          showSquare rnum (Assigned v) =
              if rnum == 1 then showString ("   " ++ show v ++ "   |")
              else showString "       |"
          showSquare rnum (Empty vs) =
              let char n = if elem n vs then Char.intToDigit n else '.'
                  start = rnum * 3 + 1
                  vals = map char [start..start+2]
              in showString (" " ++ (intersperse ' ' vals) ++ " |")


-- Parse a Sudoko board from a string.  The format of the string is:
-- 81 digits, which represent the cell values in row-major order, with
-- '0' being used to indicate an unassigned cell.
parseSudoku     :: String -> Sudoku
parseSudoku str = initialSudoku (map readValue str)

---------------------------------
-- Functions on Groups

groupSquares                   :: Sudoku -> Group -> [Element]
groupSquares sud (Row r)       = (rows sud)!r
groupSquares sud (Col c)       = (cols sud)!c
groupSquares sud (Block coord) = (blocks sud)!(blockNum coord)

allGroups :: [Group]
allGroups = [Row r | r <- [1..9]] ++
            [Col c | c <- [1..9]] ++
            [Block (r,c) | r <- [1,4,7], c <- [1,4,7]]

allGroupSquares :: Sudoku -> [[Element]]
allGroupSquares s = map (groupSquares s) allGroups

-- Get all elements in a row, column, block, connected to coord, minus
-- the input coordinate.

connectedSquares :: Sudoku -> Coord -> [Element]
connectedSquares s coord@(r,c) =
    (deleteFirstBy ((coord==).fst) (groupSquares s (Row r))) ++
    (deleteFirstBy ((coord==).fst) (groupSquares s (Col c))) ++
    (filter (\((r',c'),_) -> r /= r' && c /= c')
            (groupSquares s (Block coord)))

emptySquares :: Sudoku -> [Element]
emptySquares (Sudoku {board=b}) = filter (isEmpty.snd) (A.assocs b)

allSquares :: Sudoku -> [Element]
allSquares = A.assocs . board

assignValue :: Sudoku -> (Coord, Value) -> Sudoku
assignValue s@(Sudoku {board=b}) (c, v) =
    case b!c of
      Assigned _ -> error "Can't assign to assigned square"
      otherwise ->
          let newCells =
                  (c,Assigned v) : [(c', Empty (delete v vs)) |
                                    (c', Empty vs) <- connectedSquares s c]
          in mkSudokuFromArray $ b // newCells


removePossibleValues :: Sudoku -> [(Coord, [Value])] -> Sudoku
removePossibleValues Sudoku {board=b} to_remove =
    mkSudokuFromArray $ b // updates
    where updates = [(c, Empty $ vs' \\ vs) |
                     (c, vs) <- to_remove, Empty vs' <- [b!c]]

---- MODULE PRIVATE FUNCTIONS ----

-- Profiling showed that around 70% of the running time while solving
-- puzzles was being spent in rowSquares, colSquares, and
-- blockSquares, with the time being roughly evenly divided between
-- the 3.  In an attempt to fix this, I'm making the arrays rowCoords,
-- colCoords, and blockCoords, which, for each board cell, store the
-- set of coordinates which *Squares should use.  This way this list
-- of coordinates isn't repeatedly being recomputed each time one of
-- the *Squares functions is called, but is instead just looked up in
-- the array.

-- Read a square value
readValue :: Char -> Value
readValue = Char.digitToInt

-- Delete the first element in a list for which a predicate returns true.
deleteFirstBy          :: (a -> Bool) -> [a] -> [a]
deleteFirstBy _ []     = []
deleteFirstBy f (x:xs) = if f x then xs else x:(deleteFirstBy f xs)

blockNum :: Coord -> Int
blockNum (r,c) =
    let br = (r-1) `div` 3
        bc = (c-1) `div` 3
    in br * 3 + bc + 1

rowCoords, colCoords, blockCoords :: Int -> [Coord]

rowCoords = (a!)
    where a = A.listArray (1,9) [[(r,c) | c <- [1..9]] | r <- [1..9]]

colCoords = (a!)
    where a = A.listArray (1,9) [[(r,c) | r <- [1..9]] | c <- [1..9]]


blockCoords = (a!)
    where a = A.listArray (1,9) blocks
          blocks = [[(r,c) | r <- rs, c <- cs] |
                    rs <- [[1,2,3],[4,5,6],[7,8,9]],
                    cs <- [[1,2,3],[4,5,6],[7,8,9]]]

mkSudoku :: [Element] -> Sudoku
mkSudoku = mkSudokuFromArray . (A.array ((1,1), (9,9)))

mkSudokuFromArray :: A.Array Coord Square -> Sudoku
mkSudokuFromArray ar =
    Sudoku {board=ar,
            rows=squares rowCoords,
            cols=squares colCoords,
            blocks=squares blockCoords
           }
    where squares f = A.listArray (1,9) (map (oneGroup.f) [1..9])
          oneGroup coords = [(c, ar!c) | c <- coords]
