module Data.Multimap.SeqMultimap ( SeqMultimap
                                 , insertFirst, insertLast
                                 ) where

import Data.Multimap.Internal (Multimap, lift1)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | A multimap with 'Seq' values.
type SeqMultimap k v = Multimap k Seq v

insertFirst :: (Ord k) => k -> v -> SeqMultimap k v -> SeqMultimap k v
insertFirst k v = lift1 (v Seq.<|) k

insertLast :: (Ord k) => k -> v -> SeqMultimap k v -> SeqMultimap k v
insertLast k v = lift1 (Seq.|> v) k
