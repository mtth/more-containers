{-# LANGUAGE OverloadedLists #-}

module Spec.Multimap (
  listMultimapSpec,
  seqMultimapSpec,
  setMultimapSpec
) where

import Data.Multimap
import Data.Multimap.List
import Data.Multimap.Seq
import Data.Multimap.Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it, shouldBe)

listMultimapSpec :: Spec
listMultimapSpec = describe "ListMultimap" $ do
  let mm = fromMap $ Map.fromList [('a', [2])]
      mm' = fromMap $ Map.fromList [('a', [1,2])]
  it "cons" $ do
    cons 'a' 1 mm `shouldBe` mm'
  it "uncons" $ do
    uncons 'a' mm `shouldBe` Just (2, empty)
    uncons 'a' mm' `shouldBe` Just (1, mm)
    uncons 'b' mm `shouldBe` Nothing
  it "traverses" $ do
    let
      mm1 = [('a', Just 1), ('a', Just 2)] :: ListMultimap Char (Maybe Int)
      mm2 = [('b', Nothing)] :: ListMultimap Char (Maybe Int)
      mm3 = fromMap $ Map.singleton 'a' [1, 2]
    sequenceA mm1 `shouldBe` Just mm3
    sequenceA (mm1 <> mm2) `shouldBe` Nothing
  it "converts to set multimap" $ do
    let
      m1 = fromList [('a', 1), ('a', 1)] :: ListMultimap Char Int
      m2 = mapGroups (fmap Set.fromList) m1 :: SetMultimap Char Int
    m2 `shouldBe` fromList [('a', 1)]

seqMultimapSpec :: Spec
seqMultimapSpec = describe "SeqMultimap" $ pure () -- TODO

setMultimapSpec :: Spec
setMultimapSpec = describe "SetMultimap" $ pure () -- TODO
