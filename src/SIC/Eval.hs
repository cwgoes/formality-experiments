module SIC.Eval where

import           Data.Set
import           Protolude hiding (replace)

import           SIC.Type

eval ∷ Term → Term
eval term =
  case term of
    Var _       -> term
    Lam a b     -> Lam a (eval b)
    Era a t     -> Era a (eval t)
    Cpy _ _ _ _ -> term
    App a b ->
      let ea = eval a
          eb = eval b in
      case ea of
        Lam s t -> replace s eb t
        _       -> App ea eb

free ∷ Term → Set Symb
free (Var v)        = singleton v
free (Lam s t)      = free t \\ singleton s
free (App a b)      = free a `union` free b
free (Era s t)      = free t `union` singleton s
free (Cpy s a b t)  = singleton s `union` (free t \\ singleton b \\ singleton a)

replace ∷ Symb → Term -> Term → Term
replace sy tr term =
  case term of
    Lam a b     -> Lam a (replace sy tr b)
    App a b     -> App (replace sy tr a) (replace sy tr b)
    Var sym     -> if sym == sy then tr else Var sym
    Era s t     -> if s == sy then t else Era s (replace sy tr t)
    Cpy s a b t -> if s == sy then replace a term (replace b term t) else Cpy s a b (replace sy tr t)
