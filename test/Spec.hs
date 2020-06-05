{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Main where

import GHC.Generics
import GHC.Generics.Omit

import Test.Tasty
import Test.Tasty.HUnit

data TestData = TestData { ignored1 :: String, used :: Int, ignored2 :: [Int]  }
    deriving stock Generic
    deriving Eq via (Omit '["ignored1", "ignored2"] TestData)

basicRecords :: TestTree
basicRecords = testGroup "Basic Records"
    [ testCase "ignores fields" $ assertBool "TestData not equal" (TestData "a" 1 [] == TestData "b" 1 [3])
    , testCase "compares fields" $ assertBool "TestData equal" (TestData "a" 2 [] /= TestData "b" 1 [3])
    ]

data NestedData = NestedData { testData :: TestData, nonNested :: Maybe String }
    deriving stock Generic
    deriving Eq via (Omit '["nonNested"] NestedData)

nestedTypes :: TestTree
nestedTypes = testGroup "Nested Types"
    [ testCase "compares nested type" $ assertBool "NestedData not equal" (NestedData (TestData "a" 1 []) Nothing == NestedData (TestData "cc" 1 [4]) (Just "a"))
    ]

data SumData = Sum1 { branch :: Int, otherBranch :: String } | Sum2 { branch :: Int } | Sum3
    deriving stock Generic
    deriving Eq via (Omit '["branch"] SumData)

sumTypes :: TestTree
sumTypes = testGroup "Sum Types"
    [ testCase "ignores fields" $ assertBool "SumData not equal" (Sum1 1 "a" == Sum1 2 "a")
    , testCase "compares branches" $ assertBool "SumData equal" (Sum1 1 "a" /= Sum2 2)
    ]

main :: IO ()
main = defaultMain $ testGroup "Omit Eq"
    [ basicRecords
    , nestedTypes
    , sumTypes
    ]

data Person = Person { name :: String, age :: Int, metadata :: [String] }
    deriving stock Generic
    deriving Eq via (Omit '["age", "metadata"] Person)
