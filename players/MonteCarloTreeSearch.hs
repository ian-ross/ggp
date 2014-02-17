{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module MonteCarloTreeSearch (monteCarloTreeSearchPlayer) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe
import Data.IORef (IORef)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.IORef as IORef

import GGP.Player hiding (log)
import GGP.Utils
import Language.GDL

-- | Persistent state includes the Cp constant used in the UCT (Upper
-- Confidence Bound for Trees) algorithm and a count of the number of
-- "depth charges".
data DLState = DLState { mctsCp :: Double
                       , mctsNNodes :: Maybe (IORef Integer)
                       , mctsNPlayouts :: Maybe (IORef Integer) }

instance Default DLState where
  def = DLState { mctsCp = 1.0 / sqrt 2.0
                , mctsNNodes = Nothing
                , mctsNPlayouts = Nothing }

-- | Game state and IORef helpers.
type Game a = GGP DLState a

newIORef :: a -> Game (IORef a)
newIORef = liftIO . IORef.newIORef

readIORef :: IORef a -> Game a
readIORef = liftIO . IORef.readIORef

writeIORef :: IORef a -> a -> Game ()
writeIORef r v = liftIO $ IORef.writeIORef r v

modifyIORef' :: IORef a -> (a -> a) -> Game ()
modifyIORef' r f = liftIO $ IORef.modifyIORef' r f

-- | A single node in the MCTS tree.
--
-- Here, "actions" have type [Move] (since we maintain the role
-- ordering, this is equivalent to [(Role, Move)]) since we need to
-- apply moves for each role to make a state transition.  This is
-- consistent with carrying utilities for each role at each node.
--
-- For "normal" non-simultaneous games, this will always be [(RoleA,
-- RealMove), (RoleB, NoOp)] or [(RoleA, NoOp), (RoleB, RealMove)] so
-- there won't be an explosion in tree width.
data MCTSNode = MCTSNode
                { mctsLabel :: Integer
                  -- ^ Node label used for debugging output.
                , mctsState :: State
                  -- ^ Node state.
                , mctsAction :: Maybe [Move]
                  -- ^ Action taken from parent to arrive at node.
                , mctsActions :: IORef (Set [Move])
                  -- ^ Currently unexpanded actions.
                , mctsVisits :: IORef Integer
                  -- ^ Node visit count.
                , mctsUtils :: IORef [Integer]
                  -- ^ Node total utility vector (entries for each
                  -- player).
                , mctsChildren :: IORef [MCTSNode]
                  -- ^ Child nodes.
                , mctsParent :: Maybe MCTSNode
                  -- ^ Parent of this node.
                } deriving Eq


-- | Debug output.
printMCTSTree :: Int -> MCTSNode -> IO ()
printMCTSTree i (MCTSNode label _s _m _ms vref usref chsref p) = do
  v <- IORef.readIORef vref
  us <- IORef.readIORef usref
  chs <- IORef.readIORef chsref
  let pstr = parentStr p
  putStrLn $ replicate i ' ' ++ "N#" ++ show label ++ "{" ++ pstr ++
    "} " ++ show v ++ "=" ++ show us ++ ":"
  forM_ chs $ \n -> do
    putStrLn $ replicate i ' ' ++ show (mctsAction n) ++ " ==> "
    printMCTSTree (i + 2) n
  where parentStr Nothing = "none"
        parentStr (Just MCTSNode{..}) = show mctsLabel ++ case mctsParent of
          Nothing -> ""
          pn@(Just _) -> "->" ++ parentStr pn


-- | Main driver for UCT search algorithm.
uctSearch :: State -> Game ()
uctSearch s0 = do
  -- Root node has given state, no action to get to it and no parent.
  root <- makeNode s0 Nothing Nothing

  -- Repeatedly select a node to play out from, do the playout and
  -- back the results up to the node of the tree.
  forever $ do
    node <- treePolicy root
    delta <- defaultPolicy (mctsState node)
    backup (Just node) delta


-- | Tree expansion policy.
treePolicy :: MCTSNode -> Game MCTSNode
treePolicy v@(MCTSNode{..}) = do
  Match {..} <- get
  if isTerminal matchDB mctsState
    then
    -- Return terminal nodes immediately.
      return v
    else do
      -- If there are any unexpanded nodes below this one (as
      -- signalled by there still being entries in the "unexpanded
      -- actions" set)...
      as <- readIORef mctsActions
      if not $ S.null as
         -- ... then expand one.
        then expand v
         -- ... otherwise choose the best child according to the UCT
         -- algorithm and recurse.
        else bestChild v >>= treePolicy


-- | Choose one entry from each of a list of lists: a sort of
-- order-preserving list Cartesian product.
oneEach :: [[a]] -> [[a]]
oneEach [] = [[]]
oneEach (xs:xss) = let rs = oneEach xss
                   in concatMap (\r -> map (:r) xs) rs


-- | Expand a single child node.
expand :: MCTSNode -> Game MCTSNode
expand v@(MCTSNode{..}) = do
  Match {..} <- get
  -- Select and remove an action at random from the "unexpanded
  -- actions" set.
  as <- readIORef mctsActions
  a <- ((S.toList as) !!) <$> getRandomR (0, S.size as - 1)
  modifyIORef' mctsActions $ S.delete a

  -- Find state for new child node: here we're applying a move for
  -- each role!  We record the moves of all roles at each node and
  -- then use applyMoves (which takes a [(Role, Move)]) to go from one
  -- State to the next.
  let stnew = applyMoves matchDB mctsState (zip matchRoles a)

  -- Make and return new node.
  makeNode stnew (Just a) (Just v)


-- | Generate a new node in the search tree.
makeNode :: State
            -- ^ Node state.
         -> Maybe [Move]
            -- ^ Action taken to get to this node.
         -> Maybe MCTSNode
            -- ^ Parent node.
         -> Game MCTSNode
makeNode st a parent = do
  Match {..} <- get

  -- Label for new node.
  let (Just lref) = mctsNNodes matchExtra
  label <- readIORef lref
  modifyIORef' lref (+1)

  -- Find legal moves for new child node for each role.
  let rolemoves = map (legal matchDB st) matchRoles

  -- Form the Cartesian product of the lists of moves for each role
  -- and turn it into a set of "unexplored actions" for the new node.
  let nodeactions = S.fromList $ oneEach rolemoves

  -- Make initial IORef values for new node.
  asref <- newIORef nodeactions
  vref <- newIORef 0
  usref <- newIORef $ replicate matchNRoles 0
  chref <- newIORef []

  -- Make new node.
  let node = MCTSNode label st a asref vref usref chref parent

  -- Add new node to parent's children list.
  case parent of
    Nothing -> return ()
    Just p -> modifyIORef' (mctsChildren p) (node :)

  return node


-- | Choose the best child of a node to explore based on the UCT
-- algorithm.
bestChild :: MCTSNode -> Game MCTSNode
bestChild node = do
  Match {..} <- get

  -- Get list of children and parent node visit count.
  children <- readIORef $ mctsChildren node
  pv <- fromIntegral <$> (readIORef $ mctsVisits node)

  -- The UCT function for a node is based on the node's utility and
  -- the relative number of visits to the node and its parent.  HERE,
  -- IS (us !! matchRoleIdx) REALLY RIGHT?  IT SEEMS LIKE WE SHOULD
  -- ALWAYS BE CHOOSING THE NODES TO EXPLORE THAT ARE BEST FOR *US*,
  -- SO MAYBE IT IS CORRECT.  MAINTAINING A FULL VECTOR OF UTILITIES
  -- THROUGHOUT MEANS WE DON'T NEED TO DO ANY MINIMAXING...
  let cp = mctsCp matchExtra
      uct c = do
        us <- readIORef $ mctsUtils c
        v <- fromIntegral <$> (readIORef $ mctsVisits c)
        let u = fromIntegral $ us !! matchRoleIdx
        return $ u / v + cp * sqrt (2 * log pv / v)

  -- Return child node with best UCT score.
  cs <- mapM uct children
  return $ fst $ maximumBy (compare `on` snd) $ zip children cs


-- | Perform random playout from a given state, returning goal values
-- from first terminal node reached.
defaultPolicy :: State -> Game [Integer]
defaultPolicy st = do
  Match {..} <- get
  if isTerminal matchDB st
    then return $ orderedGoals matchDB st matchRoles
    else do
    let moves = map (legal matchDB st) matchRoles
        nmoves = map length moves
    idxs <- mapM (\n -> getRandomR (0, n-1)) nmoves
    let ms = zip matchRoles $ zipWith (!!) moves idxs
    defaultPolicy $ applyMoves matchDB st ms


-- | Back goal results up through tree.
backup :: Maybe MCTSNode -> [Integer] -> Game ()
backup mnode delta = case mnode of
  Nothing -> return ()
  Just node -> do
    modifyIORef' (mctsVisits node) (+1)
    modifyIORef' (mctsUtils node) (zipWith (+) delta)
    backup (mctsParent node) delta


bestMove :: State -> Game ()
bestMove st0 = do
  Match {..} <- get
  writeIORef (fromJust $ mctsNNodes matchExtra) 0
  writeIORef (fromJust $ mctsNPlayouts matchExtra) 0
  let as = legal matchDB st0 matchRole
  setBest $ head as
  when (length as > 1) $ uctSearch st0

finalMessage :: Game ()
finalMessage = do
  Match{..} <- get
  nnodes <- readIORef (fromJust $ mctsNNodes matchExtra)
  nplayouts <- readIORef (fromJust $ mctsNPlayouts matchExtra)
  liftIO $ putStrLn $ "Total MCTS nodes: " ++ show nnodes
  liftIO $ putStrLn $ "Total MCTS playouts: " ++ show nplayouts

initEx :: PlayerParams -> IO DLState
initEx ps = do
  nnodesref <- IORef.newIORef 0
  nplayoutsref <- IORef.newIORef 0
  let cp = case getParam "cp" ps of
        Nothing -> 1.0 / sqrt 2.0
        Just d -> (read d :: Double)
  return $ DLState cp (Just nnodesref) (Just nplayoutsref)

monteCarloTreeSearchPlayer :: Player DLState
monteCarloTreeSearchPlayer = def { initExtra = initEx
                                 , handlePlay = basicPlay bestMove
                                 , postMessage = finalMessage }
