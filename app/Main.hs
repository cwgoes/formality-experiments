module Main where

import           Protolude     hiding (reduce)

import           INet.Examples
import           INet.SIC

main ∷ IO ()
main =
  mapM_ reduceAndPrint [trivialCommute1, trivialCommute2, trivialCommute3, trivialAnnihilate1, trivialAnnihilate2, trivialAnnihilate3]

reduceAndPrint ∷ Net → IO ()
reduceAndPrint net = do
  putText $ "Pre-reduction: " <> show net
  let (reduced, steps) = reduce net
  putText $ "Reduced in " <> show steps <> " steps."
  putText $ "Post-reduction: " <> show reduced
