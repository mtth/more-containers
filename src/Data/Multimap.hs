{-# LANGUAGE CPP #-}

-- | This modules provides a strict multimap implementation. To avoid collision with Prelude
-- functions, it is recommended to import this module qualified:
--
-- > import qualified Data.Multiset as Mmap
--
-- All complexities below use /m/ for the number of distinct keys and /n/ for the total number of
-- entries in the multimap.
module Data.Multimap (
  -- * Types
  -- ** Generic
  Multimap, Group,
  -- ** Specific
  ListMultimap,
  SeqMultimap,
  SetMultimap,
  -- * Tests
  null, size,
  -- * Construction
  empty, singleton,
#if __GLASGOW_HASKELL__ >= 708
  fromList,
#endif
  fromListWith, fromGroupList, fromMap,
  -- * Access
  member, notMember,
  find, (!),
  -- * Modification
  prepend, prependMany,
  append, appendMany,
  deleteMany,
  modifyMany, modifyManyF,
  -- * Maps and filters
  mapGroups,
  filter, filterGroups,
  -- * Conversion
  toList, toGroupList, toMap,
  -- * Other
#if __GLASGOW_HASKELL__ >= 708
  inverse,
#endif
  inverseWith,
  keys, keysSet, keysMultiset
) where

import Prelude ()

import Data.Multimap.Generic
import Data.Multimap.List (ListMultimap)
import Data.Multimap.Seq (SeqMultimap)
import Data.Multimap.Set (SetMultimap)
