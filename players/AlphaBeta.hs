{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module AlphaBeta (alphaBetaPlayer) where

import Control.Monad
import Data.Function (on)
import Data.List (intercalate, maximumBy, delete)
import GGP.Player
import GGP.Utils
import Language.GDL

type Game a = GGP Int a

modLevel :: Int -> Game ()
modLevel delta = modify (\m -> m { matchExtra = matchExtra m + delta })

minscore :: Integer -> Integer -> State -> Move -> Game Integer
minscore alpha beta st m = do
  Match {..} <- get
  logMsg $ (replicate matchExtra ' ') ++
    "minscore: st=" ++ (intercalate ", " $ map prettyPrint st) ++
    " m=" ++ prettyPrint m
  let oppRole = head $ delete matchRole $ roles matchDB
      acts = legal matchDB st oppRole
      go _alp bet [] = return bet
      go alp bet (a:as) = do
        let poss = applyMoves matchDB st [(matchRole, m), (oppRole, a)]
        s <- maxscore alp bet poss
        let bet' = bet `min` s
        if bet' <= alp
          then return alp
          else go alp bet as
  modLevel 1
  retval <- go alpha beta acts
  modLevel (-1)
  logMsg $ (replicate matchExtra ' ') ++ "minscore returns " ++ show retval
  return retval

maxscore :: Integer -> Integer -> State -> Game Integer
maxscore alpha beta st = do
  Match {..} <- get
  logMsg $ (replicate matchExtra ' ') ++
    "maxscore: st=" ++ (intercalate ", " $ map prettyPrint st)
  retval <- if isTerminal matchDB st
            then return $ goal matchDB st matchRole
            else do
              modLevel 1
              let acts = legal matchDB st matchRole
                  go alp _bet [] = return alp
                  go alp bet (a:as) = do
                    s <- minscore alp bet st a
                    let alp' = alp `max` s
                    if alp' >= bet
                      then return bet
                      else go alp' bet as
              res <- go alpha beta acts
              modLevel (-1)
              return res
  logMsg $ (replicate matchExtra ' ') ++ "maxscore returns " ++ show retval
  return retval

bestMove :: State -> Game Move
bestMove st0 = do
  Match {..} <- get
  let as = legal matchDB st0 matchRole
  vs <- forM as (minscore 0 100 st0)
  let avs = zip as vs
  logMsg $ "Moves and values: " ++ show avs
  return $ fst $ maximumBy (compare `on` snd) avs

alphaBetaPlayer :: Player Int
alphaBetaPlayer = def { initExtra = 0
                      , handlePlay = basicPlay bestMove }
