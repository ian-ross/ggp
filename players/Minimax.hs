{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Minimax (minimaxPlayer) where

import Data.Function (on)
import Data.List (intercalate, maximumBy, delete)
import GGP.Player
import GGP.Utils
import Language.GDL

type Game a = GGP Int a

modNest :: Int -> Game ()
modNest delta = modify (\m -> m { matchExtra = matchExtra m + delta })

boundedMinimum :: [Integer] -> Integer
boundedMinimum [] = 100
boundedMinimum (0:_) = 0
boundedMinimum (x:xs) = x `min` boundedMinimum xs

boundedMaximum :: [Integer] -> Integer
boundedMaximum [] = 0
boundedMaximum (100:_) = 100
boundedMaximum (x:xs) = x `max` boundedMaximum xs

minscore :: State -> Move -> Game Integer
minscore st m = do
  Match {..} <- get
  logMsg $ (replicate matchExtra ' ') ++
    "minscore: st=" ++ (intercalate ", " $ map prettyPrint st) ++
    " m=" ++ prettyPrint m
  let oppRole = head $ delete matchRole $ roles matchDB
  let as = legal matchDB st oppRole
      poss = map (\a -> applyMoves matchDB st [(matchRole, m), (oppRole, a)]) as
  modNest 1
  ss <- mapM maxscore poss
  modNest (-1)
  let retval = boundedMinimum ss
  logMsg $ (replicate matchExtra ' ') ++ "minscore returns " ++ show retval
  return retval

maxscore :: State -> Game Integer
maxscore st = do
  Match {..} <- get
  logMsg $ (replicate matchExtra ' ') ++
    "maxscore: st=" ++ (intercalate ", " $ map prettyPrint st)
  retval <- if isTerminal matchDB st
            then return $ goal matchDB st matchRole
            else do
              modNest 1
              let as = legal matchDB st matchRole
                  poss = map (\a -> applyMoves matchDB st [(matchRole, a)]) as
              ss <- if matchNRoles > 1
                    then mapM (minscore st) as
                    else mapM maxscore poss
              modNest (-1)
              return $ boundedMaximum ss
  logMsg $ (replicate matchExtra ' ') ++ "maxscore returns " ++ show retval
  return retval

bestMove :: State -> Game Move
bestMove st0 = do
  Match {..} <- get
  let as = legal matchDB st0 matchRole
      poss = map (\a -> applyMoves matchDB st0 [(matchRole, a)]) as
  vs <- if matchNRoles > 1
        then mapM (minscore st0) as
        else mapM maxscore poss
  let avs = zip as vs
  logMsg $ "Moves and values: " ++ show avs
  return $ fst $ maximumBy (compare `on` snd) avs

minimaxPlayer :: Player Int
minimaxPlayer = def { initExtra = const 0, handlePlay = basicPlay bestMove }
