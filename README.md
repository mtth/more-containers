# More containers [![Stackage LTS](https://stackage.org/package/more-containers/badge/lts)](https://stackage.org/lts/package/more-containers) [![Stackage Nightly](https://stackage.org/package/more-containers/badge/nightly)](https://stackage.org/nightly/package/more-containers) [![Hackage](https://img.shields.io/hackage/v/more-containers.svg)](https://hackage.haskell.org/package/more-containers) [![Build Status](https://travis-ci.org/mtth/more-containers.svg?branch=master)](https://travis-ci.org/mtth/more-containers)

## Multisets

```haskell
> m1 = fromList [1, 2, 1] :: Multiset Int
> count 1 m1 -- 2
> m2 = replicate 3 1 -- fromList [1, 1, 1]
> insert 1 m1 == m2 <> singleton 2 -- True
```

## Multimaps

This library provides both generic and versions specialized to lists, sets, and
sequences.

```haskell
> m1 = fromList [(1, "ab"), (2, "d")] :: ListMultimap Int Char
> m1 ! 1 -- "ab"
> m1 ! 3 -- ""
> m2 = prepend 2 'c' m1 -- fromList [(1, "ab"), (2, "cd")]
> toList m2 -- [(1, 'a'), (1, 'b'), (2, 'c'), (2, 'd')]
```
