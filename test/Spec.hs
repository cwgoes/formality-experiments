module Main where

import qualified Data.Map.Strict   as M
import           Protolude         hiding (reduce)

import           SIC.INet
import           SIC.INet.Examples
import           SIC.Type

import qualified Test.Tasty        as T
import qualified Test.Tasty.HUnit  as T

main ∷ IO ()
main = T.defaultMain (T.testGroup "Tests" [
  T.testCase "trivialAnnihilate1" ((4 == (M.size $ netNodes $ fst $ reduce trivialAnnihilate1)) T.@? "trivialAnnihilate1 failed to reduce"),
  T.testCase "trivialAnnihilate2" ((4 == (M.size $ netNodes $ fst $ reduce trivialAnnihilate2)) T.@? "trivialAnnihilate2 failed to reduce")
--  T.testCase "trivialAnnihilate3" ((0 == (M.size $ netNodes $ fst $ reduce trivialAnnihilate3)) T.@? "trivialAnnihilate3 failed to reduce")
  ])
