-- | Generic multimap module.

module Data.Multimap ( Multimap, Collection
                     -- * Tests
                     , null, size
                     -- * Construction
                     , empty, singleton, fromMap
                     , fromList, fromCollectionsList
                     -- * Access
                     , (!)
                     , insert, insertAll, deleteAll
                     -- * Modification
                     , filter, filterWithKey
                     , union
                     -- * Extraction
                     , toMap, toMapWith, toList
                     -- * Other
                     , keysSet
                     , lift1
                     ) where

import Prelude ()
import Data.Multimap.Collection (Collection)
import Data.Multimap.Internal
