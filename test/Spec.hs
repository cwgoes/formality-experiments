module Main where

import           Protolude

import qualified Test.Tasty       as T
import qualified Test.Tasty.HUnit as T

main ∷ IO ()
main = T.defaultMain (T.testGroup "Tests" [
  T.testCase "trivial" (True == True T.@? "True /= True")
  ])
