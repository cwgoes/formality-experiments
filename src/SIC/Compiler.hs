module SIC.Compiler where

import           Data.Map
import           Protolude hiding (empty)

import           SIC.INet
import           SIC.Type

toNet ∷ Term → Net
toNet term = execState (buildNet term) (Net empty [])

buildNet ∷ Term → State Net ()
buildNet term = do
  case term of
    Lam _ _ -> do
      -- package + multiplex
      undefined
    App _ _ -> do
      -- link, open package
      undefined
    Era _ _ -> do
      -- erase x
      -- build net
      undefined
    Cpy _ _ _ _ -> do
      -- build net
      -- duplicate
      undefined
    Var s -> do
      node <- allocDefault Free
      linkPorts (Just $ Ptr s P) (Just $ Ptr node P)
      return ()

toNode ∷ Term → Node
toNode term =
  case term of
    Lam _ _     -> undefined
    App _ _     -> undefined
    Var s       -> FREE (Ptr s A2) -- how to represent a wire?
    Era _ _     -> undefined
    Cpy _ _ _ _ -> undefined

multiplexor ∷ Int → (Int, Net)
multiplexor n = runState (buildMultiplexor n) emptyNet

emptyNet ∷ Net
emptyNet = Net empty []

buildMultiplexor ∷ Int → State Net Int
buildMultiplexor 0 = do
  free <- allocDefault Free
  linkSlots (free, P) (free, P)
  return free
buildMultiplexor 1 = do
  zero <- buildMultiplexor 0
  free <- allocDefault Free
  linkSlots (zero, P) (free, P)
  modify reredex
  return free
buildMultiplexor n = do
  connector <- buildMultiplexor (n - 1)
  node <- allocDefault Cons
  free <- allocDefault Free
  linkSlots (node, A1) (free, P)
  linkSlots (node, P) (connector, A2)
  modify reredex
  return node

demultiplexor ∷ Int → (Int, Net)
demultiplexor n = runState (buildDemultiplexor n) emptyNet

buildDemultiplexor ∷ Int → State Net Int
buildDemultiplexor 0 = do
  free <- allocDefault Free
  linkSlots (free, P) (free, P)
  return free
buildDemultiplexor n = do
  undefined

muldemul ∷ Int → Net
muldemul n = snd $ flip runState emptyNet $ do
  multiplexor <- buildMultiplexor n
  demultiplexor <- buildDemultiplexor n
  linkSlots (multiplexor, P) (demultiplexor, P)
  return ()

package ∷ Net → Net
package net = execState buildPackage net

buildPackage ∷ State Net ()
buildPackage = undefined

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
