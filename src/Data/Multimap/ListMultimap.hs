module Data.Multimap.ListMultimap ( ListMultimap
                                  , cons, uncons
                                  ) where

import Prelude hiding (map, uncons)
import qualified Data.List as List
import Data.Multimap.Internal (Multimap(..), lift1, liftF1)

-- | A multimap with 'List' values.
type ListMultimap k v = Multimap k [] v

cons :: (Ord k) => k -> v -> ListMultimap k v -> ListMultimap k v
cons k v = lift1 (v:) k

uncons :: (Ord k, Ord v) => k -> ListMultimap k v -> Maybe (v, ListMultimap k v)
uncons k = toMaybe . liftF1 (MaybeTuple . List.uncons) k

newtype MaybeTuple a b = MaybeTuple { toMaybe :: Maybe (a,b) }

instance Functor (MaybeTuple a) where
  fmap f (MaybeTuple Nothing) = MaybeTuple Nothing
  fmap f (MaybeTuple (Just (a,b))) = MaybeTuple $ Just (a, f b)
