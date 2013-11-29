{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module DepthLimitedMinimax (depthLimitedMinimaxPlayer) where

import Data.Function (on)
import Data.List (intercalate, maximumBy, delete)
import GGP.Player
import GGP.Utils
import Language.GDL

data DLState = DLState { maxDepth :: Int, nest :: Int }

instance Default DLState where
  def = DLState { maxDepth = 2, nest = 0 }

type Game a = GGP DLState a

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

minscore :: Int -> State -> Move -> Game Integer
minscore level st m = do
  Match {..} <- get
  msg $ "minscore: st=" ++ (intercalate ", " $ map prettyPrint st) ++
    " m=" ++ prettyPrint m
  let oppRole = head $ delete matchRole $ roles matchDB
      as = legal matchDB st oppRole
      poss = map (\a -> applyMoves matchDB st [(matchRole, m), (oppRole, a)]) as
  modNest 1
  ss <- mapM (maxscore (level + 1)) poss
  modNest (-1)
  let retval = boundedMinimum ss
  msg $ "minscore returns " ++ show retval
  return retval

maxscore :: Int -> State -> Game Integer
maxscore level st = do
  Match {..} <- get
  msg $ "maxscore: st=" ++ (intercalate ", " $ map prettyPrint st)
  retval <- if isTerminal matchDB st
            then return $ goal matchDB st matchRole
            else if level >= maxDepth matchExtra
                 then return $ if matchNRoles > 1
                               then 0
                               else goal matchDB st matchRole
                 else do
                   modNest 1
                   let as = legal matchDB st matchRole
                       poss = map (\a -> applyMoves matchDB st
                                         [(matchRole, a)]) as
                   ss <- if matchNRoles > 1
                         then mapM (minscore level st) as
                         else mapM (maxscore (level + 1)) poss
                   modNest (-1)
                   return $ boundedMaximum ss
  msg $ "maxscore returns " ++ show retval
  return retval

bestMove :: State -> Game ()
bestMove st0 = do
  Match {..} <- get
  let as = legal matchDB st0 matchRole
      poss = map (\a -> applyMoves matchDB st0 [(matchRole, a)]) as
  setBest $ head as
  vs <- if matchNRoles > 1
        then mapM (minscore 0 st0) as
        else mapM (maxscore 0) poss
  let avs = zip as vs
      pravs = zip (map prettyPrint as) vs
  logMsg $ "Moves and values: " ++ show pravs
  setBest $ fst $ maximumBy (compare `on` snd) avs

initEx :: PlayerParams -> DLState
initEx ps = case getParam "maxDepth" ps of
  Nothing -> DLState 2 0
  Just d -> DLState (read d :: Int) 0

depthLimitedMinimaxPlayer :: Player DLState
depthLimitedMinimaxPlayer = def { initExtra = initEx
                                , handlePlay = basicPlay bestMove }
