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

import Debug.Trace

data DLState = DLState { maxDepth :: Int }

instance Default DLState where
  def = DLState { maxDepth = 2 }

type Game a = GGP DLState a

data MoveType = Max | Min deriving Show

data MoveTree = TerminalLeaf State Integer
              | StateLeaf State
              | Node State [(Move, MoveTree)]
                deriving (Eq, Show)

-- instance Show MoveTree where
--   show (TerminalLeaf s sc) = "T:" ++ show sc ++ "[" ++ show s ++ "]"
--   show (StateLeaf s) = "S[" ++ show s ++ "]"
--   show (Node s mts) = "N[" ++ show s ++ "]:[" ++
--                       intercalate ","
--                       (map (\(m,t) -> show m ++ "->" ++ show t) mts)
--                       ++ "]"

oppRole :: Database -> Role -> Maybe Role
oppRole db r = case delete r $ roles db of
  []      -> Nothing
  (opp:_) -> Just opp

evalMove :: M.Map State Integer -> (MoveType,MoveType) -> MoveTree -> Integer
evalMove _ _ (TerminalLeaf _ sc) = sc
evalMove dcmap _ (StateLeaf s) = dcmap M.! s
evalMove dcmap (t, ot) (Node _ mts) = cmp $ trace ("scs=" ++ show scs) scs
  where cmp = case t of
          Min -> minimum
          Max -> maximum
        scs = map (evalMove dcmap (ot, t) . snd) mts

makeMove :: M.Map State Integer -> MoveTree -> Game Move
makeMove _ (TerminalLeaf _ _) = error "Leaf state passed to makeMove!"
makeMove _ (StateLeaf _)      = error "Leaf state passed to makeMove!"
makeMove dcmap (Node _ mts) = do
  --liftIO $ putStrLn $ "let dcmap = " ++ show dcmap
  nroles <- gets matchNRoles
  --liftIO $ putStrLn $ "nroles=" ++ show nroles
  let ps = if nroles > 1 then (Min, Max) else (Max, Max)
  --liftIO $ putStrLn $ "ps=" ++ show ps
  let (as, ts) = unzip mts
  --liftIO $ putStrLn $ "as=" ++ show as
  --liftIO $ putStrLn $ "ts=" ++ show (length ts)
  let t1 = head ts
  liftIO $ putStrLn $ "let t1 = " ++ show t1
--  liftIO $ putStrLn $ "t1=" ++ show t1
  let sc1 = evalMove dcmap ps t1
  --liftIO $ putStrLn $ "sc1=" ++ show sc1
  let scs = map (evalMove dcmap ps) ts
  --liftIO $ putStrLn $ "scs=" ++ show scs
--  let avs = zip as scs
--  liftIO $ putStrLn $ "avs=" ++ show avs
  return $ fst $ head mts
--  return $ fst $ maximumBy (compare `on` snd) avs


expand :: (Role, Role) -> Int -> State -> Game MoveTree
expand (role, orole) level st = do
  Match {..} <- get
  let as = legal matchDB st role
      poss = map (\a -> applyMoves matchDB st [(role, a)]) as
  if isTerminal matchDB st
    then do
    liftIO $ putStrLn $ "Terminal leaf:"
    let v = goal matchDB st role
    liftIO $ putStrLn $ "   " ++ show v
    return $ TerminalLeaf st v
    else if level >= maxDepth matchExtra
         then do
           liftIO $ putStrLn "State leaf"
           return $ StateLeaf st
         else do
           liftIO $ putStrLn "Node:"
           ts <- mapM (expand (orole, role) (level + 1)) poss
           liftIO $ putStrLn $ "   " ++ show (length ts)
           return $ Node st $ zip as ts


leafStates :: MoveTree -> [State]
leafStates (TerminalLeaf s _) = [s]
leafStates (StateLeaf s) = [s]
leafStates (Node _ mts) = concatMap leafStates $ map snd mts

depthCharge :: Role -> (Role, Role) -> State -> Game Integer
depthCharge myrole (r, r') st = do
  Match {..} <- get
  let moves = legal matchDB st r
      nmoves = length moves
  idx <- getRandomR (0, nmoves-1)
  let move = moves !! idx
  let st' = applyMoves matchDB st [(r, move)]
  if isTerminal matchDB st'
    then return $ goal matchDB st' myrole
    else depthCharge myrole (r', r) st'

depthCharges :: Double -> (Role, Role) -> [State] -> Game (M.Map State Integer)
depthCharges maxt rs sts = do
  --liftIO $ putStrLn $ "maxt=" ++ show maxt
  conv <$> go (cycle sts) 0 0 (V.replicate nst (1, 0))
  where go :: [State] -> Int -> Int -> V.Vector (Integer, Integer)
           -> Game (V.Vector (Integer, Integer))
        go [] _ _ _ = error "Shouldn't happen"
        go (s:ss) i n v = do
          elapsed <- elapsedTime
          --liftIO $ putStrLn $ "elapsed=" ++ show elapsed
          if elapsed > maxt
            then do
            --liftIO $ putStrLn $ "Depthcharged: " ++ show n
            return v
            else do
            dc <- depthCharge (fst rs) rs s
            let (c, sc) = v V.! i
            go ss ((i + 1) `mod` nst) (n + 1) (v V.// [(i, (c+1, sc+dc))])
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
    --liftIO $ putStrLn $ "mt=" ++ show mt
    let ss = leafStates mt
    --liftIO $ putStrLn $ "length ss=" ++ show (length ss)
    dcmap <- depthCharges 4 rs ss
    --dcmap <- depthCharges (fromIntegral matchPlayClock - 0.5) rs ss
    --liftIO $ putStrLn $ "dcmap size = " ++ show (M.size dcmap)
    makeMove dcmap mt

initEx :: PlayerParams -> DLState
initEx ps = case getParam "maxDepth" ps of
  Nothing -> DLState 2
  Just d -> DLState (read d :: Int)

monteCarloPlayer :: Player DLState
monteCarloPlayer = def { initExtra = initEx
                       , handlePlay = basicPlay bestMove }
