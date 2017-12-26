{-# LANGUAGE DeriveFunctor #-}

-- | Internal multimap module; standard disclaimers apply.

module Data.Multimap.Internal ( Multimap(..)
                              , null, size
                              , empty, singleton, fromMap
                              , fromList, fromCollectionsList
                              , (!)
                              , insert, insertAll, deleteAll
                              , filter, filterWithKey
                              , union
                              , toMapWith, toList
                              , keysSet
                              , lift1, liftF1
                              ) where

import Data.Multiset (Multiset)
import Data.Multimap.Collection (Collection)
import qualified Data.Multimap.Collection as Col

import Prelude hiding (filter, foldr, null)
import qualified Prelude as Prelude
import Data.Foldable (foldl', foldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)

newtype Multimap k c v = Multimap { toMap :: Map k (c v)
                                  } deriving (Eq, Functor, Ord, Read, Show)

instance (Ord k, Semigroup (c v)) => Semigroup (Multimap k c v) where
  (<>) = union

instance (Ord k, Semigroup (c v)) => Monoid (Multimap k c v) where
  mempty = empty
  mappend = union

-- TODO: Foldable, Traversable.

null :: Multimap k c v -> Bool
null = Map.null . toMap

size :: (Collection c) => Multimap k c v -> Int
size = Map.foldl (\a c -> a + Col.size c) 0 . toMap

empty :: Multimap k c v
empty = Multimap Map.empty

singleton :: (Collection c, Ord k) => k -> v -> Multimap k c v
singleton k v = Multimap $ Map.singleton k (Col.singleton v)

fromMap :: (Collection c) => Map k (c v) -> Multimap k c v
fromMap m = Multimap $ Map.filter (not . Col.null) m

-- | Build a multimap from a list.
--
-- This method uses 'Map.insertWith' and semigroup's '<>' operator. This might
-- not be what you want, for example in the case of lists (where the order of
-- the values will be inverted). For ordered collections, prefer
-- 'fromCollectionsList'.
fromList :: (Collection c, Semigroup (c v), Ord k) => [(k,v)] -> Multimap k c v
fromList ts = Multimap $ Map.fromListWith (<>) (fmap go ts) where
  go (k,v) = (k, Col.singleton v)

-- | Build a multimap from a list of key-collection pairs.
fromCollectionsList :: (Collection c, Semigroup (c v), Ord k) => [(k, c v)] -> Multimap k c v
fromCollectionsList = fromMap . Map.fromList

(!) :: (Collection c, Ord k) => Multimap k c v -> k -> c v
(Multimap m) ! k = Map.findWithDefault Col.empty k m

member :: (Collection c, Ord k) => k -> Multimap k c v -> Bool
member k = Map.member k . toMap

count :: (Collection c, Ord k) => k -> Multimap k c v -> Int
count k = Col.size . (! k)

insertAll :: (Ord k, Semigroup (c v)) => k -> c v -> Multimap k c v -> Multimap k c v
insertAll k c (Multimap m) = Multimap $ Map.insertWith (<>) k c m

insert :: (Ord k, Collection c, Semigroup (c v)) => k -> v -> Multimap k c v -> Multimap k c v
insert k v = insertAll k (Col.singleton v)

deleteAll :: (Ord k) => k -> Multimap k c v -> Multimap k c v
deleteAll k (Multimap m) = Multimap $ Map.delete k m

filter :: (Collection c, Semigroup (c v), Ord k) => (v -> Bool) -> Multimap k c v -> Multimap k c v
filter f = filterWithKey (const f)

filterWithKey :: (Collection c, Semigroup (c v), Ord k) => (k -> v -> Bool) -> Multimap k c v -> Multimap k c v
filterWithKey f = fromList . Prelude.filter (uncurry f) . toList

union :: (Ord k, Semigroup (c v)) => Multimap k c v -> Multimap k c v -> Multimap k c v
union (Multimap m1) (Multimap m2) = Multimap $ Map.unionWith (<>) m1 m2

toMapWith :: (c v -> a) -> Multimap k c v -> Map k a
toMapWith f = fmap f . toMap

toList :: (Collection c) => Multimap k c v -> [(k,v)]
toList (Multimap m) = concat $ fmap go (Map.toList m) where
  go (k,cs) = foldr (\c a -> (k,c) : a) [] cs

keysSet :: Multimap k c v -> Set k
keysSet = Map.keysSet . toMap

keysMultiset :: Multimap k c v -> Multiset k
keysMultiset = undefined

-- | Lift an operation over a collection of values into a multimap operation.
--
-- Sample use to filter even values from a 'SetMultimap':
--
-- @
--    let ms = fromList [('a', 1), ('a', 2)] :: SetMultimap Char Int
--    lift1 (Set.filter even) 'a' ms == fromList [('a', 1)]
-- @
lift1 :: (Ord k, Collection c) => (c v -> c v)
                               -> k
                               -> Multimap k c v
                               -> Multimap k c v
lift1 f k (Multimap m) = Multimap $ Map.alter (wrap . f . unwrap) k m where
  unwrap = fromMaybe Col.empty
  wrap cs = if Col.null cs then Nothing else Just cs

-- | Applicative version of 'lift1'
liftF1 :: (Ord k, Collection c, Functor f) => (c v -> f (c v))
                                           -> k
                                           -> Multimap k c v
                                           -> f (Multimap k c v)
liftF1 f k (Multimap m) = Multimap <$> Map.alterF (fmap wrap . f . unwrap) k m where
  unwrap = fromMaybe Col.empty
  wrap cs = if Col.null cs then Nothing else Just cs
