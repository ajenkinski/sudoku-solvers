-- A test suite which uses QuickCheck instead of HUnit.  I want to
-- explore the advantages and disadvantages of QuickCheck.  It seems
-- that the main feature that QuickCheck offers is a framework for
-- generating random test data.  Each test function is written to
-- accept an input argument to test.  The QuickCheck framework then
-- calls each test function multiple times with different randomly
-- generated inputs.

-- The framework already knows how to generate random data of some
-- types.  Support for new types can be added by writing new Arbitrary
-- typeclass instances, or creating new generators.

import qualified Sudoku as S
import TestData (testBoards)

import Test.QuickCheck

import qualified Data.Array as A
import Data.Array ((!),(//))
import qualified Data.Char as Char
import qualified Data.List as L
import Data.List ((\\))
import qualified Data.Ix as Ix
import qualified Data.Maybe as M

type Board = A.Array S.Coord S.Square
data TestCase = TC String Board S.Sudoku
type Move = (S.Coord, S.Value)

instance Show TestCase where
    show (TC s b sb) =
        (showString "str = " . shows s . showChar '\n' .
         showString "expected = " . shows b . showChar '\n' .
         showString "actual =\n" . showString (S.verboseShow sb)) ""

-- Calculate the upper left corner of the block containing a coord
squareBlock :: S.Coord -> S.Coord
squareBlock (r,c) = ((r - 1) `div` 3 * 3 + 1,
                   (c - 1) `div` 3 * 3 + 1)

peerCoords       :: S.Coord -> [S.Coord]
peerCoords (r,c) =
    let (cr, cc) = squareBlock (r,c) in
    [(r,c') | c' <- [1..9], c' /= c] ++
    [(r',c) | r' <- [1..9], r' /= r] ++
    [(r',c') | r' <- [cr..cr+2], c' <- [cc..cc+2], r' /= r && c' /= c]

createBoard    :: [S.Value] -> Board
createBoard vs =
    let bounds = ((1,1), (9,9))
        init = A.listArray bounds
               (map (\v -> if v == 0 then S.Empty [] else S.Assigned v) vs)
    in A.listArray bounds (map (findValidValues init) (A.assocs init))
    where findValidValues brd (crd, sq) =
              let peerSquares = peerCoords crd in
              case sq of
                S.Assigned v -> S.Assigned v
                S.Empty _ ->
                    S.Empty ([1..9] \\ [v | S.Assigned v <- map (brd!) peerSquares])

parseBoard :: String -> Board
parseBoard s = createBoard (map Char.digitToInt s)

allCoords = Ix.range ((1,1),(9,9))

-- By overloading arbitrary for TestCase, I tell QuickCheck how to
-- generate random inputs for the test functions which have a TestCase
-- input argument.

instance Arbitrary TestCase where
    arbitrary = elements (map (\s -> TC s (parseBoard s) (S.parseSudoku s))
                          testBoards)

-- generate a random move
genMove :: Board -> Gen Move
genMove b = do let free = filter (S.isEmpty . snd) (A.assocs b)
               (crd, S.Empty vs) <- elements free
               v <- elements vs
               return (crd, v)

prop_testParseSudoku (TC s b sb) =
    all (\(crd, v) -> v == (b!crd)) (S.allSquares sb)

prop_testRowSquares (TC s b sb) = all rowEqual [1..9]
    where rowEqual r =
              let expected = [((r,c), b!(r,c)) | c <- [1..9]]
                  actual = S.groupSquares sb (S.Row r)
              in expected == actual

prop_testColSquares (TC s b sb) = all colEqual [1..9]
    where colEqual c =
              let expected = [((r,c), b!(r,c)) | r <- [1..9]]
                  actual = S.groupSquares sb (S.Col c)
              in expected == actual

prop_testBlockSquares (TC s b sb) = all blockEqual [(r,c) | r <- [1,4,7], c <- [1,4,7]]
    where blockEqual coord@(r,c) =
              let coords = [(r',c') |
                            r' <- [r..r+2],
                            c' <- [c..c+2]]
                  expected = [(co, b!co) | co <- coords]
                  actual = S.groupSquares sb (S.Block coord)
              in expected == actual

prop_testConnectedSquares (TC s b sb) = all connectedEqual allCoords
    where connectedEqual (r,c) =
              let expected = map (\crd -> (crd, b!crd)) (peerCoords (r,c))
                  actual = S.connectedSquares sb (r,c)
              in expected == actual

prop_testGetSquare (TC s b sb) = all squareEqual (A.assocs b)
    where squareEqual (crd, sq) = sq == (S.get sb crd)

prop_testAssignValue (TC s b sb) =
    forAll (genMove b) $ \(crd, val) ->
        let changes = (crd, S.Assigned val) :
                      [(crd', S.Empty $ L.delete val vals) |
                       (crd', S.Empty vals) <- [(c, b!c) | c <- peerCoords crd]]
            expected = b // changes
            actual = S.assignValue sb (crd, val)
        in (A.assocs expected) == (S.allSquares actual)


testOpts = Args { replay = Nothing,
                  maxSuccess = 100,
                  maxDiscardRatio = 500,
                  maxSize = 100,
                  chatty = True
                }

tf :: Testable f => f -> IO ()
tf = quickCheckWith testOpts

main = do tf prop_testParseSudoku
          tf prop_testRowSquares
          tf prop_testColSquares
          tf prop_testBlockSquares
          tf prop_testConnectedSquares
          tf prop_testGetSquare
          tf prop_testAssignValue
