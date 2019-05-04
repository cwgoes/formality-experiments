module SIC.Type where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import           Protolude

data Net = Net
  { netNodes :: M.Map Int Node
  , netRedex :: [(Int, Int)]
  } deriving (Show, Eq)

data Node
  = CONS Port Port Port  -- 3 slots: Primary, Left, Right
  | DUPL Port Port Port  -- 3 slots: Primary, Left, Right
  | FREE Port            -- 1 slot: Left
  deriving (Show, Eq)

data Slot = P | A1 | A2
  deriving (Show, Eq)

slot ∷ Slot → Node → Port
slot s (CONS p l r) = case s of P -> p; A1 -> l; A2 -> r;
slot s (DUPL p l r) = case s of P -> p; A1 -> l; A2 -> r;
slot _ (FREE p)     = p

setSlot ∷ Slot → Port → Node → Node
setSlot s q (CONS p l r) = case s of P -> CONS q l r; A1 -> CONS p q r; A2 -> CONS p l q;
setSlot s q (DUPL p l r) = case s of P -> DUPL q l r; A1 -> DUPL p q r; A2 -> DUPL p l q;
setSlot _ q (FREE _) = FREE q

data Kind = Cons | Dupl | Free deriving (Eq, Show)

kind ∷ Node → Kind
kind (CONS _ _ _) = Cons
kind (DUPL _ _ _) = Dupl
kind (FREE _)     = Free

defaultNode ∷ Int → Kind → Node
defaultNode n Cons = CONS (Ptr n P) (Ptr n A1) (Ptr n A2)
defaultNode n Dupl = DUPL (Ptr n P) (Ptr n A1) (Ptr n A2)
defaultNode n Free = FREE (Ptr n A1)


data Port = Ptr { toNode :: Int, toSlot :: Slot }
  deriving (Show, Eq)

type Symb = Int
type String = [Char]

data Term
  = Lam Symb Term
  | App Term Term
  | Par Term Term
  | Let Symb Symb Term Term
  | Var Symb
  deriving (Eq, Show)

instance Print Term where
  hPutStr h t = hPutStr h (T.pack $ prettyPrint t)
  hPutStrLn h t = hPutStrLn h (T.pack $ prettyPrint t)

prettyPrint ∷ Term → String
prettyPrint (Var n)   =  showVar n
prettyPrint (Lam h b) =  concat ["λ", showVar h, ".", prettyPrint b]
prettyPrint (App l r) =  concat ["(", prettyPrint l, " ", prettyPrint r, ")"]
prettyPrint (Par l r) =  concat ["(", prettyPrint l, ",", prettyPrint r, ")"]
prettyPrint (Let p q t b) = concat
  ["let (", showVar p, ",", showVar q, ") = ", prettyPrint t, " in\n", prettyPrint b]

showVar ∷ Int → String
showVar n = reverse $ go (n + 1)
  where
    go 0 = ""
    go n = chr (97 + ( (n - 1) `mod` 26)) : go ((n - 1) `div` 26)
