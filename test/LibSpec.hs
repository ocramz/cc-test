{-# language OverloadedStrings #-}
module Main where

import Control.Monad (void)

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
  checkParallel $ Group "Test.Example" [
      ("prop_reverse", prop_reverse)
    ]

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs




-- * Tree generators

genTree :: Int -- ^ maximum length of name strings
        -> Gen Tree
genTree n = Gen.recursive Gen.choice nrg rg
  where
    nrg = [tyA n [], Tree_TypeB <$> tyB n []]
    rg = [
      Gen.subtermM (genTree n) (\x -> tyA n [x]) ,
      Tree_TypeB <$> Gen.subtermM (genTypeB n) (\x -> tyB n [x])
      ]

tyA :: Int -> [Tree] -> Gen Tree
tyA n x = Tree_TypeA <$> genNodeInfo n <*> genString n <*> pure x


genTypeB :: Int -> Gen TypeB
genTypeB n = Gen.recursive Gen.choice nrg rg
  where
    nrg = [tyB n []]
    rg = [Gen.subtermM (genTypeB n) (\x -> tyB n [x]) ]

tyB :: Int -> [TypeB] -> Gen TypeB
tyB n x = TypeB <$> genCost <*> genNodeName n <*> pure x


-- * Basic generators

genNodeInfo :: Int -> Gen NodeInfo
genNodeInfo n = NodeInfo <$> genCost <*> genNodeName n

genCost :: Gen Cost
genCost = Cost <$> pure 0 -- <$> Gen.float (Range.constantFrom 0 0 10.0)

-- | names are nonempty strings of length smaller than n
genNodeName :: Int -> Gen NodeName
genNodeName n = NodeName <$> genString n

genString :: Int -> Gen String 
genString n = Gen.string (Range.linear 1 n) Gen.alpha
