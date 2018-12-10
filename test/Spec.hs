import Data.Multimap (Multimap)
import qualified Data.Multimap as Mmap
import Data.Multimap.ListMultimap (ListMultimap)
import qualified Data.Multimap.ListMultimap as Mmap
import Data.Multimap.SetMultimap (SetMultimap)
import qualified Data.Multimap.SetMultimap as Mmap
import Data.Multiset (Multiset)
import qualified Data.Multiset as Mset
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Multiset" $ do
    it "Empty should have size 0" $ do
      Mset.size Mset.empty `shouldBe` 0
    it "Singleton should have size 1" $ do
      Mset.size (Mset.singleton 'a') `shouldBe` 1
    it "From list" $ do
      Mset.fromList ['a', 'b', 'a'] `shouldBe` Mset.fromCountsList [('a', 2), ('b', 1)]
    it "Incr" $ do
      let m = Mset.fromList ['a']
      Mset.insert 'a' m `shouldBe` Mset.mapCounts (*2) m
    it "Most common" $ do
      let m = Mset.fromList ['a', 'b', 'b', 'b', 'c', 'd']
      Mset.mostCommon m `shouldBe` ['b']
  describe "ListMultimap" $ do
    let mm = Mmap.fromCollectionsList [('a', [2])]
        mm' = Mmap.fromCollectionsList [('a', [1,2])]
    it "cons" $ do
      Mmap.cons 'a' 1 mm `shouldBe` mm'
    it "uncon" $ do
      Mmap.uncons 'a' mm `shouldBe` Just (2, Mmap.empty)
      Mmap.uncons 'a' mm' `shouldBe` Just (1, mm)
      Mmap.uncons 'b' mm `shouldBe` Nothing
  describe "SetMultimap" $ do
    let mm = Mmap.fromList [('a', 2)]
        mm' = Mmap.fromList [('a', 1), ('a',2)]
    it "delete" $ do
      Mmap.delete 'a' 1 mm' `shouldBe` mm
