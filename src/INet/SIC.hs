module INet.SIC where

import qualified Data.Map.Strict as M
import           GHC.Generics
import           Protolude       hiding (reduce)

data Net = Net {
  netNodes :: M.Map Int Node,
  netRedex :: [(Int, Int)],
  netIndex :: Int
} deriving (Show, Eq, Generic)

data Kind
  = Constructor
  | Duplicator
  | Eraser
  | Wire

  deriving (Show, Eq, Generic)

data Node = Node {
  nodeKind  :: Kind,
  nodePorts :: M.Map Int Port
} deriving (Show, Eq, Generic)

data Port =
  Pointer { pointerIndex :: Int, pointerSlot :: Int } |
  Free Text

  deriving (Show, Eq, Generic)

makeNet ∷ [(Kind, [Port])] → Net
makeNet nodes = setRedexes $ Net {
  netNodes = M.fromList $ zip [0..] $ map (\(k, p) -> Node k (M.fromList $ zip [0..] p)) nodes,
  netRedex = [],
  netIndex = length nodes
  }

setRedexes ∷ Net → Net
setRedexes net = net { netRedex = findRedexes net }

findRedexes ∷ Net → [(Int, Int)]
findRedexes net =
  let nodes   = netNodes net
      indices = concatMap (\i -> zip (repeat i) [i .. netIndex net - 1]) [0 .. netIndex net - 1]
  in flip filter indices $ \(a, b) ->
    let nodeA = nodes M.! a
        port0 = nodePorts nodeA M.! 0
    in case port0 of
      Pointer index slot -> index == b && slot == 0
      _                  -> False

allocNode ∷ Kind → [Port] → State Net Int
allocNode kind ports = do
  let node = Node kind (M.fromList $ zip [0..] ports)
  addr <- state (\n -> (netIndex n, n { netIndex = netIndex n + 1 }))
  modify (\n -> n { netNodes = M.insert addr node $ netNodes n })
  return addr

freeNode ∷ Int → State Net ()
freeNode addr = do
  modify (\n -> n { netNodes = M.delete addr $ netNodes n })
  return ()

enterPort ∷ Int → Int → State Net (Maybe Port)
enterPort addr port = do
  net <- get
  return $ case M.lookup addr (netNodes net) of
    Just node -> nodePorts node M.!? port
    _         -> Nothing

setPort ∷ Int → Int → Port → State Net ()
setPort addr slot port = modify $ \net ->
  let node    = netNodes net M.! addr
      updated = node { nodePorts = M.insert slot port $ nodePorts node }
  in net { netNodes = M.insert addr updated $ netNodes net }

linkPorts ∷ Maybe Port → Maybe Port → State Net ()
linkPorts ptrA ptrB =
  case (ptrA, ptrB) of
    (Just pa@(Pointer indexA slotA), Just pb@(Pointer indexB slotB)) -> do
      setPort indexA slotA pb
      setPort indexB slotB pa
      when (slotA == 0 && slotB == 0) $ modify (\n -> n { netRedex = (indexA, indexB) : netRedex n })
    (Just (Pointer indexA slotA), Just pb) ->
      setPort indexA slotA pb
    (Just pa, Just (Pointer indexB slotB)) ->
      setPort indexB slotB pa
    (Just pa, Just pb) -> do
      _ <- allocNode Wire [pa, pb]
      return ()
    _ -> return ()

unlinkPort ∷ Int → Int → State Net ()
unlinkPort addr port = do
  other <- enterPort addr port
  case other of
    Just (Pointer index slot) -> do
      reverse <- enterPort index slot
      case reverse of
        Just (Pointer index' slot') | index' == addr && slot' == port -> do
          setPort addr port $ Pointer addr port
          setPort index slot $ Pointer index slot
        _ -> return ()
    _ -> return ()

rewrite ∷ (Int, Int) → State Net ()
rewrite (addrA, addrB) = do
  net <- get

  let nodes = netNodes net
      nodeA = nodes M.! addrA
      nodeB = nodes M.! addrB
      kindA = nodeKind nodeA
      kindB = nodeKind nodeB

  if
    | kindA == kindB -> do
      aux1destA <- enterPort addrA 1
      aux1destB <- enterPort addrB 1
      linkPorts aux1destA aux1destB
      aux2destA <- enterPort addrA 2
      aux2destB <- enterPort addrB 2
      linkPorts aux2destA aux2destB

    | otherwise -> do
      addrP <- allocNode kindB []
      addrQ <- allocNode kindB []
      addrR <- allocNode kindA []
      addrS <- allocNode kindA []
      linkPorts (Just $ Pointer addrR 1) (Just $ Pointer addrP 1)
      linkPorts (Just $ Pointer addrS 1) (Just $ Pointer addrP 2)
      linkPorts (Just $ Pointer addrR 2) (Just $ Pointer addrQ 1)
      linkPorts (Just $ Pointer addrS 2) (Just $ Pointer addrQ 2)
      a1dest <- enterPort addrA 1
      a2dest <- enterPort addrA 2
      b1dest <- enterPort addrB 1
      b2dest <- enterPort addrB 2
      linkPorts (Just $ Pointer addrP 0) a1dest
      linkPorts (Just $ Pointer addrQ 0) a2dest
      linkPorts (Just $ Pointer addrR 0) b1dest
      linkPorts (Just $ Pointer addrS 0) b2dest

  mapM_ (unlinkPort addrA) [0 .. 2]
  mapM_ (unlinkPort addrB) [0 .. 2]

  freeNode addrA
  unless (addrA == addrB) (freeNode addrB)

reduce ∷ Net → (Net, Int)
reduce net =
  let go net count =
        case netRedex net of
          []  -> (net, count)
          r:rs ->
            let newNet = execState (rewrite r) (net { netRedex = rs })
            in go newNet (count + 1)
  in go net 0
