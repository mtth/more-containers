module Data.Multimap.SetMultimap ( SetMultimap
                                 , delete, map
                                 ) where

import Prelude hiding (map)
import Data.Multimap.Internal (Multimap(..), lift1)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A multimap with 'Set' values.
type SetMultimap k v = Multimap k Set v

map :: (Ord k, Ord v1 , Ord v2) => (v1 -> v2) -> SetMultimap k v1 -> SetMultimap k v2
map f (Multimap m) = Multimap $ fmap (Set.map f) m

delete :: (Ord k, Ord v) => k -> v -> SetMultimap k v -> SetMultimap k v
delete k v = lift1 (Set.delete v) k
