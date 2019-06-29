# More containers [![Stackage LTS](https://stackage.org/package/more-containers/badge/lts)](https://stackage.org/lts/package/more-containers) [![Stackage Nightly](https://stackage.org/package/more-containers/badge/nightly)](https://stackage.org/nightly/package/more-containers) [![Hackage](https://img.shields.io/hackage/v/more-containers.svg)](https://hackage.haskell.org/package/more-containers) [![Build Status](https://travis-ci.org/mtth/more-containers.svg?branch=master)](https://travis-ci.org/mtth/more-containers)

+ [`Multiset`](https://hackage.haskell.org/package/more-containers/docs/Data-Multiset.html)

  > [..] a multiset (aka bag or mset) is a modification of the concept of a set
  > that, unlike a set, allows for multiple instances for each of its elements.
  > -- [Wikipedia](https://en.wikipedia.org/wiki/Multiset):

  ```haskell
  > m1 = fromList [1, 2, 1] :: Multiset Int
  > count 1 m1 -- 2
  > m2 = replicate 3 1 -- fromList [1, 1, 1]
  > insert 1 m1 == m2 <> singleton 2 -- True
  ```

+ [`Multimap`](https://hackage.haskell.org/package/more-containers/docs/Data-Multimap.html)

  > [...] a multimap (sometimes also multihash or multidict) is a
  > generalization of a map or associative array abstract data type in which
  > more than one value may be associated with and returned for a given key. --
  > [Wikipedia](https://en.wikipedia.org/wiki/Multimap)

  ```haskell
  > m1 = fromList [(1, "ab"), (2, "d")] :: ListMultimap Int Char
  > m1 ! 1 -- "ab"
  > m1 ! 3 -- ""
  > m2 = prepend 2 'c' m1 -- fromList [(1, "ab"), (2, "cd")]
  > toList m2 -- [(1, 'a'), (1, 'b'), (2, 'c'), (2, 'd')]
  ```
