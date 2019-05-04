module SIC.Compiler where

import           Protolude

import           SIC.INet
import           SIC.Type

toNet ∷ Term → Net
toNet = undefined


--toNode :: Term -> (Kind, [Port])
--toNode t =
-- where
--  go ns t = case t of
--    Lam h f -> (Constructor, [ Pointer ? 0, Pointer ? 1, Pointer f 2 ]
--    App l r -> (Constructor, [ Pointer ? 0, Pointer ? 1, Pointer r 2 ]
--    App l r -> (Duplicator, [ Pointer ? 0, Pointer 1 l, Pointer 2 r ]
--    Let p q v n ->  (Duplicator, [Pointer 0 v, Pointer 1 p, Pointer 2 q]
--    Var n ->


fromNet ∷ Net → Term
fromNet = undefined
