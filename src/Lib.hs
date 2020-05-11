{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module Lib where

import Data.String (IsString(..))

import qualified Data.Set as S (fromList, intersection, toList)

data Tree = Tree_TypeA NodeInfo String [Tree]
          | Tree_TypeB TypeB deriving (Eq, Show)

data NodeInfo = NodeInfo { cost :: Cost
                         , nodeInfoName :: NodeName } deriving (Eq, Show)

newtype NodeName = NodeName String deriving (Eq, Ord, Show, IsString)

data TypeB = TypeB Cost NodeName [TypeB] deriving (Eq, Show)

newtype Cost = Cost Float deriving (Eq, Show)


{- | Implement the function getCommonNodeNamesExceptBepa.
- The function should return all node names that exists in both the trees (the name does not need to be in the same place in the tree)
- Names that contains the string  "Bepa" (case sensitive) should never be included in the result
-}
getCommonNodeNamesExceptBepa :: Tree -> Tree -> [NodeName]
getCommonNodeNamesExceptBepa t1 t2 = S.toList $
                                     S.intersection (legit t1) (legit t2)
  where
    legit = S.fromList . filter (/= "Bepa") . names

names :: Tree -> [NodeName]
names = go
  where
    go = \case
      Tree_TypeA (NodeInfo _ n) _ ts -> n : concatMap go     ts
      Tree_TypeB (TypeB _ n tbs)     -> n : concatMap namesB tbs

namesB :: TypeB -> [NodeName]
namesB (TypeB _ n tbs) = n : concatMap namesB tbs
