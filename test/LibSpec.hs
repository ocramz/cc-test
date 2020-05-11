{-# language OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.List (any)

import qualified Data.CaseInsensitive as CI (mk)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Lib

main :: IO ()
main = void tests


{- |
Implement "property-based tests" to the function getCommonNodeNamesExceptBepa
- Write generators for all the types described above
- Identify as many properties as possible and write tests for them
- See if you can find ways to update the types (possibly with new types) to reduce the number of tests needed to ensure the properties
- Finally, write a test for the property  "Nodes with the name "bepa" (case insensitive) should never be present in the results"
   This property should fail without changing the implementation.
-}

tests :: IO Bool
tests =
  checkParallel $ Group "Lib" [
      ("prop_noBepaCI",
       withDiscards 100000 $ withTests 100000 $ prop_noBepaCI),
      ("prop_noBepaNodesCI",
       withDiscards 100 $ withTests 10 $ prop_noBepaNodesCI)
    ]


prop_noBepaNodesCI :: Property
prop_noBepaNodesCI = property $ do
  t1 <- forAll genTree
  t2 <- forAll genTree
  let nns = (\(NodeName x) -> CI.mk x) `map` getCommonNodeNamesExceptBepa t1 t2
  assert (none (== CI.mk "bepa") nns)

none :: Foldable t => (a -> Bool) -> t a -> Bool
none q = not . any q

prop_noBepaCI :: Property
prop_noBepaCI = property $ do
  (NodeName x) <- forAll genNodeName
  CI.mk x /== CI.mk "bepa"



-- | Tree generator
genTree :: Gen Tree
genTree = Gen.recursive Gen.choice nrg rg
  where
    -- non-recursive generators
    nrg = [
      tyA [],
      Tree_TypeB <$> tyB []
      ]
    -- recursive generators
    rg = [
      Gen.subtermM genTree (\x -> tyA [x]) ,
      Tree_TypeB <$> Gen.subtermM genTypeB (\x -> tyB [x])
      ]

tyA :: [Tree] -> Gen Tree
tyA x = Tree_TypeA <$> genNodeInfo <*> genString <*> pure x

genTypeB :: Gen TypeB
genTypeB = Gen.recursive Gen.choice nrg rg
  where
    nrg = [tyB []]
    rg = [Gen.subtermM genTypeB (\x -> tyB [x]) ]

tyB :: [TypeB] -> Gen TypeB
tyB x = TypeB <$> genCost <*> genNodeName <*> pure x


-- * Basic generators

-- | Generate a 'NodeInfo'
genNodeInfo :: Gen NodeInfo
genNodeInfo = NodeInfo <$> genCost <*> genNodeName

-- | Generate a 'Cost'
genCost :: Gen Cost
genCost = Cost <$> pure 0 -- TBD

-- | Generate a 'NodeName'
genNodeName :: Gen NodeName
genNodeName = NodeName <$> genString

-- | Generate a _very_ restricted string value
genString :: Gen String
genString = do
  let f (a:b:_) = a == 'b' && b == 'e'
      f _ = False
  Gen.filter f $ Gen.string (Range.singleton 4) Gen.alpha

-- genString = Gen.string (Range.linear 1 n) Gen.alpha
