module Data.Multimap.Collection ( Collection
                                , null, empty, singleton, size
                                ) where

import Prelude hiding (foldr, null)
import qualified Prelude as Prelude
import Data.Foldable (foldl', foldr)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- | Lower bound for multimap values.
--
-- An alternative could have been to use 'Applicative' but that would have
-- precluded common implementations (e.g. 'Set').
class Foldable c => Collection c where

  -- | The empty collection.
  empty :: c v

  -- | A singleton collection.
  singleton :: v -> c v

  null :: c v -> Bool
  null = foldr (\_ _ -> False) True

  size :: c v -> Int
  size cs = foldl' (\a _ -> a + 1) 0 cs

instance Collection Set where
  empty = Set.empty
  singleton = Set.singleton
  null = Set.null
  size = Set.size

instance Collection [] where
  empty = []
  singleton v = [v]
  null = Prelude.null
  size = Prelude.length

instance Collection Seq where
  empty = Seq.empty
  singleton = Seq.singleton
  null = Seq.null
  size = Seq.length
