# Build

You need to have GHC v7.10 or higher and Cabal. Then,

1. Download this package,
2. `cd` into the base directory, then
3. run `cabal sandbox init && cabal install --only-dependencies && cabal run`.

The command will build all dependencies automatically and run the compiled executable.
