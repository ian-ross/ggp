{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module DepthLimitedMinimax (depthLimitedMinimaxPlayer) where

import Control.Monad
import Data.Function (on)
import Data.List (intercalate, maximumBy, delete)
import GGP.Player
import GGP.Utils
import Language.GDL

data DLState = DLState { maxDepth :: Int, nest :: Int }

instance Default DLState where
  def = DLState { maxDepth = 2, nest = 0 }

type Game a = GGP DLState a

data MoveType = Max | Min deriving Show

modNest :: Int -> Game ()
modNest delta = modExtra (\e -> e { nest = nest e + delta })

msg :: String -> Game ()
msg s = do
  e <- gets matchExtra
  logMsg $ replicate (nest e) ' ' ++ s

boundedMinimum :: [Integer] -> Integer
boundedMinimum [] = 100
boundedMinimum (0:_) = 0
boundedMinimum (x:xs) = x `min` boundedMinimum xs

boundedMaximum :: [Integer] -> Integer
boundedMaximum [] = 0
boundedMaximum (100:_) = 100
boundedMaximum (x:xs) = x `max` boundedMaximum xs

oppRole :: Database -> Role -> Maybe Role
oppRole db r = case delete r $ roles db of
  []      -> Nothing
  (opp:_) -> Just opp

makeMove :: MoveType -> Int -> State -> Game Integer
makeMove t level st = do
  Match {..} <- get
  msg $ "makeMove " ++ show t ++ ": st=" ++
    (intercalate ", " $ map prettyPrint st)

  let orole = oppRole matchDB matchRole
      (nextt, findRes, role) = case (t, orole) of
        (_, Nothing) ->    (Max, boundedMaximum, matchRole)
        (Max, _) ->        (Min, boundedMaximum, matchRole)
        (Min, Just opp) -> (Max, boundedMinimum, opp)
      as = legal matchDB st role
      poss = map (\a -> applyMoves matchDB st [(role, a)]) as

  retval <- if isTerminal matchDB st
            then return $ goal matchDB st matchRole
            else if level >= maxDepth matchExtra
                 then return $ if matchNRoles > 1
                               then 0
                               else goal matchDB st matchRole
                 else do
                   modNest 1
                   ss <- mapM (makeMove nextt (level + 1)) poss
                   modNest (-1)
                   when ((nest matchExtra) == 0) $
                     msg $ "Moves and values: " ++ show ss
                   return $ findRes ss

  msg $ "makeMove " ++ show t ++ " returns " ++ show retval
  return retval

bestMove :: State -> Game Move
bestMove st0 = do
  Match {..} <- get
  let as = legal matchDB st0 matchRole
      poss = map (\a -> applyMoves matchDB st0 [(matchRole, a)]) as
  vs <- mapM (makeMove Max 0) poss
  let avs = zip as vs
  msg $ "Moves and values: " ++ show avs
  return $ fst $ maximumBy (compare `on` snd) avs

initEx :: PlayerParams -> DLState
initEx ps = case getParam "maxDepth" ps of
  Nothing -> DLState 2 0
  Just d -> DLState (read d :: Int) 0

depthLimitedMinimaxPlayer :: Player DLState
depthLimitedMinimaxPlayer = def { initExtra = initEx
                                , handlePlay = basicPlay bestMove }
