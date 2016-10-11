import Sudoku
import Test.HUnit
import qualified Char
import qualified Ix
import qualified Array as A
import Array ((!),(//))
import List (nub,delete)
import Maybe (mapMaybe)

-- String representation of the sudoku problem I'll test with

boardStr =
    ("000000010" ++
     "400000000" ++
     "020000000" ++
     "000050407" ++
     "008000300" ++
     "001090000" ++
     "300400200" ++
     "050100000" ++ 
     "000806000")

-- expected square values after parsing above board
boardSquares = A.listArray ((1,1),(9,9)) squareVals
    where squareVals = map parseVal stringVals
          parseVal sv =
              if sv!!0 == '-' then Assigned (Char.digitToInt (sv!!1))
              else Empty (map Char.digitToInt sv)
          stringVals =
              [ "56789",			-- cell 1,1
                "36789",			-- cell 1,2
                "35679",			-- etc...
                "235679",
                "234678",
                "2345789",
                "56789",
                "-1",
                "2345689",

                -- row 2
                "-4",
                "136789",
                "35679",
                "235679",
                "123678",
                "1235789",
                "56789",
                "2356789",
                "235689",

                -- row 3
                "156789",
                "-2",
                "35679",
                "35679",
                "134678",
                "1345789",
                "56789",
                "3456789",
                "345689",

                -- row 4
                "269",
                "369",
                "2369",
                "236",
                "-5",
                "1238",
                "-4",
                "2689",
                "-7",

                -- row 5
                "25679",
                "4679",
                "-8",
                "267",
                "12467",
                "1247",
                "-3",
                "2569",
                "12569",

                -- row 6
                "2567",
                "3467",
                "-1",
                "2367",
                "-9",
                "23478",
                "568",
                "2568",
                "2568",

                -- row 7
                "-3",
                "16789",
                "679",
                "-4",
                "7",
                "579",
                "-2",
                "56789",
                "15689",

                -- row 8
                "26789",
                "-5",
                "24679",
                "-1",
                "237",
                "2379",
                "6789",
                "346789",
                "34689",

                -- row 9
                "1279",
                "1479",
                "2479",
                "-8",
                "237",
                "-6",
                "1579",
                "34579",
                "13459" ]

allCoords = Ix.range ((1,1),(9,9))

board = parseSudoku boardStr

testParseSudoku = "testParseSudoku" ~: mapM_ assertCellValue (allSquares board)
    where assertCellValue (coord, val) =
              assertEqual ("cell " ++ show coord) (boardSquares!coord) val


testRowSquares = "testRowSquares" ~: mapM_ assertRowEqual allCoords
    where assertRowEqual coord@(r,_) = 
              let expected = [((r,c), boardSquares!(r,c)) | c <- [1..9]]
                  actual = groupSquares board (Row r)
              in assertEqual ("row for coord " ++ show coord) expected actual

testColSquares = "testColSquares" ~: mapM_ assertColEqual allCoords
    where assertColEqual coord@(_,c) = 
              let expected = [((r,c), boardSquares!(r,c)) | r <- [1..9]]
                  actual = groupSquares board (Col c)
              in assertEqual ("col for coord " ++ show coord) expected actual

testBlockSquares = "testBlockSquares" ~: mapM_ assertBlockEqual allCoords
    where assertBlockEqual coord@(r,c) =
              let firstRow = ((r-1) `div` 3) * 3 + 1
                  firstCol = ((c-1) `div` 3) * 3 + 1
                  coords = [(r',c') |
                            r' <- [firstRow..firstRow+2],
                            c' <- [firstCol..firstCol+2]]
                  expected = [(co, boardSquares!co) | co <- coords]
                  actual = groupSquares board (Block coord)
              in assertEqual ("block for coord " ++ show coord) expected actual

testConnectedSquares = "testConnectedSquares" ~: mapM_ assertConnectedEqual allCoords
    where assertConnectedEqual coord@(r,c) =
              let expected = filter ((/= coord).fst) (nub (groupSquares board (Row r) ++
                                                           groupSquares board (Col c) ++
                                                           groupSquares board (Block coord)))
                  actual = connectedSquares board coord
              in assertEqual ("connected squares for " ++ show coord) expected actual

testAllSquares = "testAllSquares" ~: mapM_ assertSquareEqual (A.assocs boardSquares)
    where assertSquareEqual (coord, sq) =
              assertEqual (show coord) sq (get board coord)

testAssignValue = "testAssignValue" ~: assertAssignValue (1,6) 4
     where assertAssignValue coord val =
               let connectedCoords = map fst (connectedSquares board coord)
                   changes = (coord, Assigned val) :
                             (mapMaybe
                              (\crd -> do vals <- possibleValues (boardSquares!crd)
                                          return $ (crd, Empty $ delete val vals))
                              connectedCoords)
                   newBoardSquares = boardSquares // changes
                   newBoard = assignValue board (coord, val)
               in assertEqual "" (A.assocs newBoardSquares) (allSquares newBoard)


-- Add other tests to this list
allTests = TestList [ testParseSudoku
                    , testRowSquares
                    , testColSquares
                    , testBlockSquares
                    , testConnectedSquares
                    , testAllSquares
                    , testAssignValue
                    ]

main = runTestTT allTests
