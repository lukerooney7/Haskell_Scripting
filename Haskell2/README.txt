To run the program:
    >source /usr/local/haskell/activate.sh
    cabal update
    cabal install QuickCheck
    cabal build
    cabal run

To run tests:
    ghc --make RunTests.hs -o tests
    ./tests
