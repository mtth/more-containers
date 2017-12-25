import Data.Multiset (Multiset)
import qualified Data.Multiset as Mset
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Exception (evaluate)

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
