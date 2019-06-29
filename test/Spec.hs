{-# LANGUAGE OverloadedLists #-}

import Spec.Multimap
import Spec.Multiset

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  multisetSpec
  listMultimapSpec
  seqMultimapSpec
  setMultimapSpec
