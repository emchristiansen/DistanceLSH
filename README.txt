Warning: pre-alpha code

Performs approximate nearest neighbor given just a distance function using a locality sensitive hashing (LSH) inspired technique. 
The underlying data does not need to be embedded in a vector space (unlike conventional LSH), and the distance function only needs to be a metric.

To build:

1) cd to project root
2) > runhaskell Setup configure
3) > runhaskell Setup build
4) > sudo runhaskell Setup install

See for more info: http://www.haskell.org/haskellwiki/Cabal/How_to_install_a_Cabal_package

Author: Eric Christiansen
License: This software is entirely in the public domain.
