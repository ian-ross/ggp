{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module MonteCarlo (monteCarloPlayer) where

import Control.Monad
import Data.Function (on)
import Data.Ord
import Data.List (sortBy)
import Data.Maybe
import Data.IORef (IORef)
import qualified Data.IORef as IORef

import GGP.Player
import GGP.Utils
import Language.GDL

data DLState = DLState { maxDepth :: Int
                       , mcCount :: Maybe (IORef Integer) }

instance Default DLState where
  def = DLState { maxDepth = 2, mcCount = Nothing }

type Game a = GGP DLState a

data MoveType = Max | Min deriving (Eq, Show)

data MTree = TerminalLeaf { _mtLabel :: Int
                          , _mtState :: State
                          , mtUtils :: IORef [Double] }
             -- ^ State and goal values.
           | StateLeaf { _mtLabel :: Int
                       , _mtState :: State
                       , _mtVisits :: IORef Integer
                       , mtUtils :: IORef [Double]
                       , mtParent :: Maybe MTree }
             -- ^ State, visit count, mean utilities, parent.
           | Node { _mtLabel :: Int
                  , _mtState :: State
                  , mtUtils :: IORef [Double]
                  , mtChildren :: [([Move], MTree)]
                  , mtParent :: Maybe MTree }
             -- ^ State, utilities, children, parent.
           deriving Eq

newIORef :: a -> Game (IORef a)
newIORef = liftIO . IORef.newIORef

readIORef :: IORef a -> Game a
readIORef = liftIO . IORef.readIORef

writeIORef :: IORef a -> a -> Game ()
writeIORef r v = liftIO $ IORef.writeIORef r v

modifyIORef' :: IORef a -> (a -> a) -> Game ()
modifyIORef' r f = liftIO $ IORef.modifyIORef' r f

utils :: MTree -> Game [Double]
utils (TerminalLeaf _ _ gsref) = readIORef gsref
utils (StateLeaf _ _ _ usref _) = readIORef usref
utils (Node _ _ usref _ _) = readIORef usref

parentStr :: MTree -> String
parentStr n = case n of
  TerminalLeaf _ _ _ -> "terminal"
  StateLeaf l _ _ _ p -> pstr l p
  Node l _ _ _ p -> pstr l p
  where pstr l p = show l ++ case p of
          Nothing -> ""
          Just pn -> "->" ++ parentStr pn

-- printMTree :: Int -> MTree -> IO ()
-- printMTree i (TerminalLeaf l s gsref) = do
--   gs <- IORef.readIORef gsref
--   putStrLn $ replicate i ' ' ++ "T#" ++ show l ++
--     " [" ++ pretty1 s ++ "->" ++ show gs ++ "]"
-- printMTree i (StateLeaf l s vref usref p) = do
--   v <- IORef.readIORef vref
--   us <- IORef.readIORef usref
--   let pstr = case p of
--         Nothing -> "none"
--         Just p -> parentStr p
--   putStrLn $ replicate i ' ' ++
--     "S#" ++ show l ++ "{" ++ pstr ++ "} [" ++ pretty1 s ++
--     " (" ++ show v ++ "->" ++ show us ++ ")]"
-- printMTree i (Node l s usref mts p) = do
--   us <- IORef.readIORef usref
--   let pstr = case p of
--         Nothing -> "none"
--         Just p -> parentStr p
--   putStrLn $ replicate i ' ' ++ "N#" ++ show l ++ "{" ++ pstr ++
--     "} [" ++ pretty1 s ++
--     " -> " ++ show us ++ "]:"
--   forM_ mts $ \(ms, t) -> do
--     putStrLn $ replicate i ' ' ++ show ms ++ " ==> "
--     printMTree (i + 2) t


printMTree :: Int -> MTree -> IO ()
printMTree i (TerminalLeaf l s gsref) = do
  gs <- IORef.readIORef gsref
  putStrLn $ replicate i ' ' ++ "T#" ++ show l ++ show gs
printMTree i (StateLeaf l s vref usref p) = do
  v <- IORef.readIORef vref
  us <- IORef.readIORef usref
  let pstr = case p of
        Nothing -> "none"
        Just p -> parentStr p
  putStrLn $ replicate i ' ' ++
    "S#" ++ show l ++ "{" ++ pstr ++ "} [" ++ show v ++ "->" ++ show us ++ "]"
printMTree i (Node l s usref mts p) = do
  us <- IORef.readIORef usref
  let pstr = case p of
        Nothing -> "none"
        Just p -> parentStr p
  putStrLn $ replicate i ' ' ++ "N#" ++ show l ++ "{" ++ pstr ++
    "} " ++ show us ++ ":"
  forM_ mts $ \(ms, t) -> do
    putStrLn $ replicate i ' ' ++ show ms ++ " ==> "
    printMTree (i + 2) t

oneEach :: [[a]] -> [[a]]
oneEach [] = [[]]
oneEach (xs:xss) = let rs = oneEach xss
                   in concatMap (\r -> map (:r) xs) rs

expand :: IORef Int -> Int -> Maybe MTree -> State -> Game MTree
expand lref ply parent st = do
  Match {..} <- get
  if isTerminal matchDB st
    then do
    l <- readIORef lref
    modifyIORef' lref (+1)
    gsref <- newIORef $ map fromIntegral $ orderedGoals matchDB st matchRoles
    return $ TerminalLeaf l st gsref
    else if ply == maxDepth matchExtra
         then do
           vref <- newIORef 0
           usref <- newIORef $ replicate matchNRoles 0
           l <- readIORef lref
           modifyIORef' lref (+1)
           return $ StateLeaf l st vref usref parent
         else do
           let combine r as = map (\a -> (r, a)) as
               allas = zipWith combine matchRoles $
                       map (legal matchDB st) matchRoles
               ms = oneEach allas
               poss = map (applyMoves matchDB st) ms
           chs <- mapM (expand lref (ply + 1) Nothing) poss
           l' <- readIORef lref
           modifyIORef' lref (+1)
           usref <- newIORef $ replicate matchNRoles 0
           let addParent _ tn@(TerminalLeaf _ _ _) = tn
               addParent p nn@(StateLeaf _ _ _ _ _) = nn { mtParent = Just p }
               addParent p nn@(Node _ _ _ ch _) =
                 let n' = nn { mtChildren = ch', mtParent = Just p }
                     ch' = map (\(mvs, mt) -> (mvs, addParent n' mt)) ch
                 in n'
               mts = zip (map (map snd) ms) (map (addParent n) chs)
               n = Node l' st usref mts parent
           allus <- mapM utils chs
           writeIORef usref $ foldr1 (zipWith max) allus
           return n

leafStates :: MTree -> [MTree]
leafStates (TerminalLeaf _ _ _) = []
leafStates s@(StateLeaf _ _ _ _ _) = [s]
leafStates (Node _ _ _ mts _) = concatMap leafStates $ map snd mts

randomPlay :: State -> Game [Integer]
randomPlay st = do
  Match {..} <- get
  if isTerminal matchDB st
    then return $ orderedGoals matchDB st matchRoles
    else do
    let moves = map (legal matchDB st) matchRoles
        nmoves = map length moves
    idxs <- mapM (\n -> getRandomR (0, n-1)) nmoves
    let ms = zip matchRoles $ zipWith (!!) moves idxs
    randomPlay $ applyMoves matchDB st ms

propagate :: [Double] -> Maybe MTree -> Game ()
propagate newus (Just (Node _ _ usref _ parent)) = do
  us <- readIORef usref
  let us' = zipWith max us newus
  writeIORef usref us'
  propagate us' parent
propagate _ _ = return ()

chooseBestMove :: [([Move], [Double])] -> Game Move
chooseBestMove mvus = do
  Match{..} <- get
  let i = matchRoleIdx
      subs = sortBy (compare `on` (Down . (!! i) . snd)) mvus
      maxs = snd (head subs) !! i
      poss = takeWhile (\(_, ss) -> ss !! i == maxs) subs
      others = map (\(ms, ss) -> (ms, take i ss ++ drop (i + 1) ss)) poss
      sothers = sortBy (compare `on` snd) others
  -- liftIO $ putStrLn $ "poss=" ++ show poss
  -- liftIO $ putStrLn $ "others=" ++ show others
  -- liftIO $ putStrLn $ "sothers=" ++ show sothers
  let mv = (fst (head sothers)) !! i
  -- liftIO $ putStrLn $ "mv=" ++ show mv
  return mv

mcUpdates :: MTree -> [MTree] -> Game ()
mcUpdates top@(Node _ _ _ _ _) ss = do
  Match{..} <- get
  let ns = length ss
  when (ns > 0) $ do
    is <- getRandomR (0, ns-1)
    let (StateLeaf _ s vref usref parent) = ss !! is
    sample <- randomPlay s
    liftIO $ putStrLn $ "sample=" ++ show sample
    v <- readIORef vref
    us <- readIORef usref
    let vr = fromIntegral v
        us' = zipWith (\u x -> (vr * u + fromIntegral x) / (vr + 1)) us sample
    writeIORef usref us'
    writeIORef vref (v + 1)
    modifyIORef' (fromJust $ mcCount matchExtra) (+1)
    propagate us' parent
    --liftIO $ printMTree 0 top
  let (mvs, submts) = unzip $ mtChildren top
  subus <- mapM (readIORef . mtUtils) submts
  let mvus = zip mvs subus
  mv <- chooseBestMove mvus
  setBest mv
  mcUpdates top ss
mcUpdates _ _ = return ()

bestMove :: State -> Game ()
bestMove st0 = do
  Match {..} <- get
  writeIORef (fromJust $ mcCount matchExtra) 0
  let as = legal matchDB st0 matchRole
  setBest $ head as
  when (length as > 1) $ do
    lref <- newIORef 1
    mt <- expand lref 0 Nothing st0
    let ss = leafStates mt
    liftIO $ putStrLn $ "# of leaf states: " ++ show (length ss)
    mcUpdates mt ss

finalMessage :: Game ()
finalMessage = do
  Match{..} <- get
  c <- readIORef (fromJust $ mcCount matchExtra)
  liftIO $ putStrLn $ "Total MC samples: " ++ show c

initEx :: PlayerParams -> IO DLState
initEx ps = do
  cref <- IORef.newIORef 0
  return $ case getParam "maxDepth" ps of
    Nothing -> DLState 2 (Just cref)
    Just d -> DLState (read d :: Int) (Just cref)

monteCarloPlayer :: Player DLState
monteCarloPlayer = def { initExtra = initEx
                       , handlePlay = basicPlay bestMove
                       , postMessage = finalMessage }
