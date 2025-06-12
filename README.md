# `ghc-ar-quickcheck`

This repo contains an out-of-tree `QuickCheck` testsuite for
[`GHC.SysTools.Ar`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/SysTools/Ar.hs),
which used to be ridiculously broken
([#26120](https://gitlab.haskell.org/ghc/ghc/-/issues/26120)).

The testsuite contains simple roundtrip tests of archive
parsing/writing logic, as well as roundtrip tests that invoke an
external `ar` program to extract and re-archive before parsing it
back.

To run the testsuite, run `cabal test`. It passes when `ar` is GNU
`ar`. On macOS, `ar` is BSD `ar` and you need to adjust the test, read
the [test](test/Main.hs) for details. TODO: `llvm-ar` doesn't work
yet.

To test an updated version of `GHC.SysTools.Ar`, copy the updated
module to [`src/GHC/SysTools/Ar.hs`](src/GHC/SysTools/Ar.hs).
