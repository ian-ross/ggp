{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module MonteCarlo (monteCarloPlayer) where

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (intercalate, maximumBy, delete)
import qualified Data.Map as M
import qualified Data.Vector as V
import GGP.Player
import GGP.Utils
import Language.GDL

data DLState = DLState { maxDepth :: Int }

instance Default DLState where
  def = DLState { maxDepth = 2 }

type Game a = GGP DLState a

data MoveType = Max | Min deriving Show

data MoveTree = TerminalLeaf State Integer
              | StateLeaf State
              | Node State [(Move, MoveTree)]
                deriving Eq

instance Show MoveTree where
  show (TerminalLeaf s sc) = "T:" ++ show sc ++ "[" ++ show s ++ "]"
  show (StateLeaf s) = "S[" ++ show s ++ "]"
  show (Node s mts) = "N[" ++ show s ++ "]:\n  [" ++
                      intercalate ",\n   "
                      (map (\(m,t) -> show m ++ "->" ++ show t) mts)
                      ++ "]"

oppRole :: Database -> Role -> Maybe Role
oppRole db r = case delete r $ roles db of
  []      -> Nothing
  (opp:_) -> Just opp

evalMove :: M.Map State Integer -> (MoveType,MoveType) -> MoveTree
         -> Integer
evalMove _ _ (TerminalLeaf _ sc) = sc
evalMove dcmap _ (StateLeaf s) = dcmap M.! s
evalMove dcmap (t, ot) (Node _ mts) =
  cmp $ map (evalMove dcmap (ot, t) . snd) mts
  where cmp = case t of
          Min -> minimum
          Max -> maximum

makeMove :: M.Map State Integer -> MoveTree -> Game Move
makeMove _ (TerminalLeaf _ _) = error "Leaf state passed to makeMove!"
makeMove _ (StateLeaf _)      = error "Leaf state passed to makeMove!"
makeMove dcmap (Node _ mts) = do
  nroles <- gets matchNRoles
  let ps = if nroles > 1 then (Min, Max) else (Max, Max)
      (as, ts) = unzip mts
      scs = map (evalMove dcmap ps) ts
  return $ fst $ maximumBy (compare `on` snd) $ zip as scs


expand :: (Role, Role) -> Int -> State -> Game MoveTree
expand (role, orole) ply st = do
  Match {..} <- get
  let as = legal matchDB st role
      as' = legal matchDB st orole
      poss = concatMap (\a ->
                         map (\a' ->
                               applyMoves matchDB st [(role, a), (orole,a')])
                         as')
             as
  if isTerminal matchDB st
    then return $ TerminalLeaf st $ goal matchDB st role
    else if ply >= maxDepth matchExtra
         then return $ StateLeaf st
         else (Node st . zip as) <$> mapM (expand (orole, role) (ply + 1)) poss


leafStates :: MoveTree -> [State]
leafStates (TerminalLeaf _ _) = []
leafStates (StateLeaf s) = [s]
leafStates (Node _ mts) = concatMap leafStates $ map snd mts

depthCharge :: Role -> (Role, Role) -> State -> Game Integer
depthCharge myrole (r, r') st = do
  Match {..} <- get
  let moves = legal matchDB st r
      nmoves = length moves
      moves' = legal matchDB st r'
      nmoves' = length moves'
  idx <- getRandomR (0, nmoves-1)
  idx' <- getRandomR (0, nmoves'-1)
  let move = moves !! idx
      move' = moves' !! idx'
  let st' = applyMoves matchDB st [(r, move), (r', move')]
  if isTerminal matchDB st'
    then return $ goal matchDB st' myrole
    else depthCharge myrole (r', r) st'

depthCharges :: Double -> (Role, Role) -> [State] -> Game (M.Map State Integer)
depthCharges maxt rs sts =
  if null sts
  then return M.empty
  else conv <$> go (V.fromList sts) 0 (V.replicate nst (1, 1))
  where go :: V.Vector State -> Int -> V.Vector (Integer, Integer)
           -> Game (V.Vector (Integer, Integer))
        go ss n v = do
          elapsed <- elapsedTime
          if elapsed > maxt
            then do
            liftIO $ putStrLn $ "# of depthcharges: " ++ show n
            return v
            else do
            i <- getRandomR (0, V.length ss - 1)
            dc <- depthCharge (fst rs) rs (ss V.! i)
            let (c, sc) = v V.! i
            go ss (n + 1) (v V.// [(i, (c+1, sc+dc))])
        nst = length sts
        conv = M.fromList . zip sts . V.toList . V.map mean
        mean (c, sc) = sc `div` c

bestMove :: State -> Game Move
bestMove st0 = do
  Match {..} <- get
  let as = legal matchDB st0 matchRole
  if length as == 1
    then return $ head as
    else do
    let rs = (matchRole, maybe matchRole id $ oppRole matchDB matchRole)
    mt <- expand rs 0 st0
    let ss = leafStates mt
    liftIO $ putStrLn $ "# of leaf states: " ++ show (length ss)
    dcmap <- depthCharges (fromIntegral matchPlayClock - 0.5) rs ss
    makeMove dcmap mt

initEx :: PlayerParams -> DLState
initEx ps = case getParam "maxDepth" ps of
  Nothing -> DLState 2
  Just d -> DLState (read d :: Int)

monteCarloPlayer :: Player DLState
monteCarloPlayer = def { initExtra = initEx
                       , handlePlay = basicPlay bestMove }
