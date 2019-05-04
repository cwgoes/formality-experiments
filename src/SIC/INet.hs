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
  deDuplicate $ (toNode . slot P) <$> (M.filterWithKey isRedex nodes)
  where
    isRedex _ (CONS (Ptr _ P) _ _) = True
    isRedex _ (DUPL (Ptr _ P) _ _) = True
    isRedex n (FREE (Ptr n' _))    = n == n'
    isRedex _ _                    = False
    deDuplicate = Set.toList . (M.foldrWithKey f Set.empty)
    f k v s
      | Set.member (v, k) s = s
      | otherwise = (Set.insert (k, v) s)

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
  do ib <- allocDefault Free; linkSlots (ia,sa) (ib,A1)
linkPorts Nothing (Just (Ptr ib sb))            =
  do ia <- allocDefault Free; linkSlots (ia,A1) (ib,sb)
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
      a1dest <- enterPort iA A1
      b1dest <- enterPort iB A1
      linkPorts a1dest b1dest
      a2dest <- enterPort iA A2
      b2dest <- enterPort iB A2
      linkPorts a2dest b2dest
    | otherwise -> do
      iP <- allocDefault (kind b)
      iQ <- allocDefault (kind b)
      iR <- allocDefault (kind a)
      iS <- allocDefault (kind a)
      linkSlots (iS, A1) (iP, A2)
      linkSlots (iR, A2) (iQ, A1)
      linkSlots (iS, A2) (iQ, A2)
      linkSlots (iR, A1) (iP, A1)
      a1dest <- enterPort iA A1
      a2dest <- enterPort iA A2
      b1dest <- enterPort iB A1
      b2dest <- enterPort iB A2
      linkPorts (Just $ Ptr iP P) a1dest
      linkPorts (Just $ Ptr iQ P) a2dest
      linkPorts (Just $ Ptr iR P) b1dest
      linkPorts (Just $ Ptr iS P) b2dest

  mapM_ (\x -> unlinkPort (iA, x)) [P,A1,A2] >> freeNode iA
  unless (iA == iB) (mapM_ (\x -> unlinkPort (iB, x)) [P,A1,A2] >> freeNode iB)

reduce ∷ Net → (Net, Int)
reduce net =
  let go net count =
        case netRedex net of
          []  -> (net, count)
          r:rs ->
            let newNet = execState (rewrite r) (net { netRedex = rs })
            in go newNet (count + 1)
  in go net 0
