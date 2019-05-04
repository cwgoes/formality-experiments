module Main where

import qualified Data.Text.IO      as TIO
import           Protolude         hiding (reduce)
import           Text.Megaparsec   (parse)

import           SIC.INet
import           SIC.INet.Examples
import           SIC.Parser
import           SIC.Type

main ∷ IO ()
main = mapM_ reduceAndPrint
    [ trivialCommute1
    , trivialCommute2
    , trivialCommute3
    , trivialAnnihilate1
    , trivialAnnihilate2
    ]

reduceAndPrint ∷ Net → IO ()
reduceAndPrint net = do
  putLText $ "Pre-reduction:\n" <> show net
  let (reduced, steps) = reduce net
  putLText $ "Reduced in " <> show steps <> " steps."
  putLText $ "Post-reduction:\n" <> show reduced <> "\n"

readTermFromFile ∷ FilePath → IO (Maybe Term)
readTermFromFile f = do
  x <- TIO.readFile f
  let a = (parse term f x)
  return $ either (const Nothing) Just a

term_3plus2 ∷ Term
term_3plus2 = Let 26 27 (Lam 15 (App (App (Var 15) (Lam 16 (Lam 17 (Lam 18 (Lam 19 (App (Var 18) (App (App (Var 26) (Var 16)) (Var 17)))))))) (Lam 20 (Var 20)))) (App (App (Var 27) (Lam 1 (Lam 2 (App (Var 1) (Lam 3 (Lam 4 (App (Var 3) (Lam 5 (Lam 6 (App (Var 5) (Lam 7 (Lam 8 (Var 8))))))))))))) (Lam 9 (Lam 10 (App (Var 9) (Lam 11 (Lam 12 (App (Var 11) (Lam 13 (Lam 14 (Var 14))))))))))

