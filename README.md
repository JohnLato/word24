[![Build Status](https://secure.travis-ci.org/JohnLato/word24.png?branch=master)](http://travis-ci.org/JohnLato/word24)

This library implements 24-bit word and int types suitable for use in vector
processing.

Storable implementations are provided, however as they use a size and alignment
of 3 bytes care should be taken to ensure they are compatible if used in
conjunction with the FFI.

These types use unboxed data and GHC primitives, and are unlikely to be
compatible with other Haskell compilers.

INSTALLATION INSTRUCTIONS

This library uses the Hackage/Cabal build system.  You will need a working
Haskell compiler and appropriate build system.  This is most easily met
by installing the Haskell Platform.  The following command will install
the library:

cabal install iteratee

This library is pure Haskell, and should install on any system with a suitable
Haskell compiler with no extra steps required.  In particular, POSIX-compatible,
Mac OSX, and Windows should all be supported.

INSTALLATION OPTIONS:

This library supports the following cabal flags:
  splitBase (default enabled): use the split-up base package.

