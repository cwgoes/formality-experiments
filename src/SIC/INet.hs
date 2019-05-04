module SIC.INet where

import qualified Data.Map.Strict as M
import qualified Data.Set        as Set
import           Protolude       hiding (reduce)

import           SIC.Type

makeNet ∷ [Node] → Net
makeNet nodes =
  let n = M.fromList $ zip [0..] nodes
  in Net n (findRedexes n)

findRedexes ∷ M.Map Int Node → [(Int, Int)]
findRedexes nodes =
  go Set.empty $ M.toList $ (toNode . slot P) <$> nodes
  where
    isFree n = maybe False (\x -> kind x == Free) $ M.lookup n nodes
    go s ((a, b):xs)
      | isFree a && a == b = go (Set.insert (a, b) s) xs
      | isFree a || isFree b || Set.member (b, a) s = go s xs
      | otherwise = go (Set.insert (a, b) s) xs
    go s [] = Set.toList s

allocNode ∷ Node → State Net Int
allocNode node = do
  addr <- gets $ (maybe 0 ((+1) . fst)) . M.lookupMax . netNodes
  modify (\n -> n { netNodes = M.insert addr node $ netNodes n })
  return addr

allocDefault ∷ Kind → State Net Int
allocDefault k = do
  i <- gets $ (maybe 0 ((+1) . fst)) . M.lookupMax . netNodes
  modify (\n -> n { netNodes = M.insert i (defaultNode i k) $ netNodes n })
  return i

freeNode ∷ Int → State Net ()
freeNode addr = modify (\n -> n { netNodes = M.delete addr $ netNodes n })

enterPort ∷ Int → Slot → State Net (Maybe Port)
enterPort addr s = do
  net <- get
  return $ (slot s) <$> M.lookup addr (netNodes net)

setPort ∷ Int → Slot → Port → State Net ()
setPort addr s port = modify $ \net ->
  net { netNodes = M.adjust (setSlot s port) addr $ netNodes net }

linkSlots ∷ (Int, Slot) → (Int, Slot) → State Net ()
linkSlots (ia, sa) (ib, sb) = do
  setPort ia sa $ Ptr ib sb
  setPort ib sb $ Ptr ia sa
  when (sa == P && sb == P) $
    modify (\n -> n { netRedex = (ia, ib) : netRedex n })

linkPorts ∷ Maybe Port → Maybe Port → State Net ()
linkPorts (Just (Ptr ia sa)) (Just (Ptr ib sb)) = linkSlots (ia, sa) (ib, sb)
linkPorts (Just (Ptr ia sa)) Nothing            =
  do ib <- allocDefault Free; linkSlots (ia,sa) (ib,L)
linkPorts Nothing (Just (Ptr ib sb))            =
  do ia <- allocDefault Free; linkSlots (ia,L) (ib,sb)
linkPorts _ _                                   = return ()

unlinkPort ∷ (Int, Slot) → State Net ()
unlinkPort (ia, sa) = do
  b <- enterPort ia sa
  case b of
    Just (Ptr ib sb) -> do
      a' <- enterPort ib sb
      case a' of
        Just (Ptr ia' sa') | ia' == ia && sa' == sa -> do
          setPort ia sa $ Ptr ia sa
          setPort ib sb $ Ptr ib sb
        _ -> return ()
    _ -> return ()

rewrite ∷ (Int, Int) → State Net ()
rewrite (iA, iB) = do
  a <- gets $ ((M.! iA) . netNodes)
  b <- gets $ ((M.! iB) . netNodes)
  if
    | kind a == kind b -> do
      aLdest <- enterPort iA L
      bLdest <- enterPort iB L
      linkPorts aLdest bLdest
      aRdest <- enterPort iA R
      bRdest <- enterPort iB R
      linkPorts aRdest bRdest
    | otherwise -> do
      iP <- allocDefault (kind b)
      iQ <- allocDefault (kind b)
      iR <- allocDefault (kind a)
      iS <- allocDefault (kind a)
      linkSlots (iR, R) (iP, L)
      linkSlots (iS, R) (iP, R)
      linkSlots (iR, L) (iQ, L)
      linkSlots (iS, L) (iQ, R)
      aLdest <- enterPort iA L
      aRdest <- enterPort iA R
      bLdest <- enterPort iB L
      bRdest <- enterPort iB R
      linkPorts (Just $ Ptr iP P) aLdest
      linkPorts (Just $ Ptr iQ P) aRdest
      linkPorts (Just $ Ptr iR P) bLdest
      linkPorts (Just $ Ptr iS P) bRdest

  mapM_ (\x -> unlinkPort (iA, x)) [P,L,R] >> freeNode iA
  unless (iA == iB) (mapM_ (\x -> unlinkPort (iB, x)) [P,L,R] >> freeNode iB)

reduce ∷ Net → (Net, Int)
reduce net =
  let go net count =
        case netRedex net of
          []  -> (net, count)
          r:rs ->
            let newNet = execState (rewrite r) (net { netRedex = rs })
            in go newNet (count + 1)
  in go net 0
