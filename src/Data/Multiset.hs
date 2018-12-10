{-# LANGUAGE TupleSections #-}

-- | A simple multiset implementation
--
-- All complexities below use /m/ for the number of distinct elements and /n/
-- for the total number of elements.

module Data.Multiset ( Multiset
                    -- * Tests
                     , null, size, distinctSize
                    -- * Construction
                     , empty, singleton, fromMap, fromMap'
                     , fromList, fromCountsList, fromCountsList'
                    -- * Accessors
                     , member, notMember
                     , (!), count
                    -- * Update
                     , incr, incr', insert, remove, remove'
                     , filter, filterCounts
                     , map, mapCounts
                    -- * Combination
                     , max, min, sum, unionWith, difference, intersectionWith
                     , toList, toCountsList, toAscCountsList, toDescCountsList
                     -- * Other
                     , elems, mostCommon
                     ) where

import Prelude hiding (filter, foldr, map, max, min, null, sum)
import qualified Prelude as Prelude
import Control.Monad (guard)
import Data.Foldable (foldl', foldr)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (traverse)

-- | A multiset
newtype Multiset v = Multiset { getMultiset :: Map v Int
                              } deriving (Eq, Ord, Read, Show)

instance (Ord v) => Semigroup (Multiset v) where
  (<>) = sum

instance (Ord v) => Monoid (Multiset v) where
  mempty = empty
  mappend = sum

instance Foldable Multiset where
  foldMap f (Multiset m) = Map.foldMapWithKey go m where
    go v n = mconcat $ replicate n (f v)

-- | /O(1)/ Whether a multiset is empty.
null :: Multiset v -> Bool
null = Map.null . getMultiset

-- | The total number of elements in the multiset.
--
-- /O(m)/ Note that this isn't the number of /distinct/ elements,
-- 'distinctSize' provides it.
size :: Multiset v -> Int
size = Map.foldl' (\a c -> a + c) 0 . getMultiset

-- | /O(1)/ The number of distinct elements in the multiset.
distinctSize :: Multiset v -> Int
distinctSize = Map.size . getMultiset

-- Construction

-- | /O(1)/ The empty multiset.
empty :: Multiset v
empty = Multiset Map.empty

-- | /O(1)/ A multiset with a single element.
singleton :: v -> Multiset v
singleton v = Multiset $ Map.singleton v 1

-- | /O(m * log m)/ Build a multiset from a map.
--
-- Negative counts are ignored; see 'fromMap'' for a stricter version.
fromMap :: (Integral a, Ord v) => Map v a -> Multiset v
fromMap = Multiset . Map.map fromIntegral . Map.filter (> 0)

-- | /O(m * log m)/ Build a multiset from a map.
--
-- If at least one of the counts is negative, this method will return
-- 'Nothing'.
fromMap' :: (Integral a, Ord v) => Map v a -> Maybe (Multiset v)
fromMap' m = fromMap <$> traverse go m where
  go n = if n < 0 then Nothing else Just (fromIntegral n)

-- | /O(n * log n)/ Build a multiset from a list.
fromList :: (Ord v) => [v] -> Multiset v
fromList vs = Multiset $ Map.fromListWith (+) (fmap (,1) vs)

-- | /O(m * log m)/ Build a multiset from a list of counts.
--
-- Counts of duplicate entries are added together.
fromCountsList :: (Integral a, Ord v) => [(v,a)] -> Multiset v
fromCountsList = fromMap . Map.fromListWith (+)

-- | /O(m * log m)/ Build a multiset from a list of counts.
--
-- Counts of duplicate entries are added together. Returns 'Nothing' if the
-- total count for any element is negative.
fromCountsList' :: (Integral a, Ord v) => [(v,a)] -> Maybe (Multiset v)
fromCountsList' = fromMap' . Map.fromListWith (+)

-- Access

-- | /O(log m)/ Whether the element is present at least once.
member :: (Ord v) => v -> Multiset v -> Bool
member v = Map.member v . getMultiset

-- | /O(log m)/ Whether the element is not present.
notMember :: (Ord v) => v -> Multiset v -> Bool
notMember v = Map.notMember v . getMultiset

-- | /O(1)/ The number of times the element is present in the multiset.
--
-- 0 if absent.
count :: (Ord v) => v -> Multiset v -> Int
count v = Map.findWithDefault 0 v . getMultiset

-- | /O(1)/ Infix version of 'count'.
(!) :: (Ord v) => Multiset v -> v -> Int
(!) = flip count

-- | /O(log m)/ Increment the count of element.
--
-- The increment can be negative (removing elements). Resulting negative counts
-- are considered 0 (see 'incr'' for a stricter implementation)..
incr :: (Ord v) => Int -> v -> Multiset v -> Multiset v
incr n v (Multiset m) = Multiset $ Map.alter (wrap . (+n) . unwrap) v m where
  unwrap = fromMaybe 0
  wrap n = if n <= 0 then Nothing else Just n

-- | /O(log m)/ Increment the count of element, enforcing that any returned
-- multiset has non-negative counts. If a resulting count would have become
-- negative, this function returns 'Nothing'
incr' :: (Ord v) => Int -> v -> Multiset v -> Maybe (Multiset v)
incr' n v mm@(Multiset m) = Multiset <$> do
  let n' = (count v mm) + n
  guard $ n' >= 0
  return $ (if n' == 0 then Map.delete v else Map.insert v n') m

-- | /O(log m)/ Insert a single element.
insert :: (Ord v) => v -> Multiset v -> Multiset v
insert = incr 1

-- | /O(log m)/ Remove a single element. Does nothing if the element isn't
-- present.
remove :: (Ord v) => v -> Multiset v -> Multiset v
remove = incr (-1)

-- | Remove a single element. Returns 'Nothing' if the element wasn't already in.
remove' :: (Ord v) => v -> Multiset v -> Maybe (Multiset v)
remove' = incr' (-1)

-- | Standard value filter.
filter :: (Ord v) => (v -> Bool) -> Multiset v -> Multiset v
filter f (Multiset m) = Multiset $ Map.filterWithKey (\v _ -> f v) m

-- | Filter on counts.
filterCounts :: (Ord v) => (Int -> Bool) -> Multiset v -> Multiset v
filterCounts f (Multiset m) = Multiset $ Map.filter f m

-- | Map on the multiset's values.
map :: (Ord v1, Ord v2) => (v1 -> v2) -> Multiset v1 -> Multiset v2
map f (Multiset m) = Multiset $ Map.mapKeys f m

-- | Map on the multiset's counts.
mapCounts :: (Ord v) => (Int -> Int) -> Multiset v -> Multiset v
mapCounts f (Multiset m) = fromMap $ Map.map f m

-- | Convenience methods to get the sum of two multisets.
sum :: (Ord v) => Multiset v -> Multiset v -> Multiset v
sum = unionWith' (+)

-- | Convenience methods to get the max of two multisets.
max :: (Ord v) => Multiset v -> Multiset v -> Multiset v
max = unionWith' Prelude.max

-- | Convenience methods to get the min of two multisets.
min :: (Ord v) => Multiset v -> Multiset v -> Multiset v
min = intersectionWith Prelude.min

-- | Generic union method.
unionWith :: (Ord v) => (Int -> Int -> Int) -> Multiset v -> Multiset v -> Multiset v
unionWith f ms1@(Multiset m1) ms2@(Multiset m2) = fromMap m' where
  vs = Set.union (Map.keysSet m1) (Map.keysSet m2)
  go v = (v, f (count v ms1) (count v ms2))
  m' = Map.fromList $ fmap go $ Set.toList vs

-- | Generic intersection method.
intersectionWith :: (Ord v) => (Int -> Int -> Int) -> Multiset v -> Multiset v -> Multiset v
intersectionWith f (Multiset m1) (Multiset m2) = fromMap $ Map.intersectionWith f m1 m2

-- | The first set minus the second. Resulting negative counts are ignored.
difference :: (Ord v) => Multiset v -> Multiset v -> Multiset v
difference (Multiset m1) (Multiset m2) = Multiset $ Map.differenceWith go m1 m2 where
  go n1 n2 = let n = n1 - n2 in if n > 0 then Just n else Nothing

-- | /O(1)/ Convert the multiset to a map of counts.
toMap :: Multiset v -> Map v Int
toMap (Multiset m) = m

-- | Convert the multiset to a list; elements will be repeated according to their count.
toList :: Multiset v -> [v]
toList = concat . fmap (uncurry (flip replicate)) . Map.toList . getMultiset

-- | Convert the multiset to a list of values and associated counts. The
-- entries are in undefined order; see 'toAscCountsList' and 'toDescCountsList'
-- for sorted versions.
toCountsList :: Multiset v -> [(v,Int)]
toCountsList = Map.toList . getMultiset

-- | Convert the multiset into a list of values and counts, from least common
-- to most.
toAscCountsList :: Multiset v -> [(v,Int)]
toAscCountsList = sortOn snd . toCountsList

-- | Convert the multiset into a list of values and counts, from most common
-- to least.
toDescCountsList :: Multiset v -> [(v,Int)]
toDescCountsList = sortOn (negate . snd) . toCountsList

-- Other

-- | /O(m)/ The 'Set' of all elements in the multiset.
elems :: Multiset v -> Set v
elems = Map.keysSet . getMultiset

-- | /O(m)/ The list of all elements with the highest count.
mostCommon :: Multiset v -> [v]
mostCommon ms = case toDescCountsList ms of
  [] -> []
  ((v, c) : ts) -> v : (fmap fst . takeWhile ((== c) . snd) $ ts)

-- Internal

unionWith' :: (Ord v) => (Int -> Int -> Int) -> Multiset v -> Multiset v -> Multiset v
unionWith' f (Multiset m1) (Multiset m2) = fromMap $ Map.unionWith f m1 m2
