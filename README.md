# Sudoku Solvers

This project contains a couple of Sudoku solvers, written in Haskell.  
LogicSolver uses logic rules similar to what a person would use to solve puzzles,
and AlgoXSolver uses the constraint satisfaction algorithm, Algorithm X, to
solve puzzles.

This project uses cabal to build.

```bash
# build everything
cabal build

# run unit tests
cabal test
```

Run LogicSolver:

```bash
# see options
cabal run LogicSolver -- -h

# solve a puzzle
cabal run LogicSolver -- -p 000000010400000000020000000000050407008000300001090000300400200050100000000806000

# Solve all puzzles in min-sudoku-puzzles.txt in parallel, using 4 threads
cabal run LogicSolver -- -P min-sudoku-puzzles.txt +RTS -N4 -RTS
```

Run AlgoXSolver similarly, just substituting AlgoXSolver for LogicSolver in the
above commands.
