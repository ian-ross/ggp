{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module AlphaBeta (alphaBetaPlayer) where

import Control.Monad
import Data.List (intercalate, delete)
import GGP.Player
import GGP.Utils
import Language.GDL

type Game a = GGP Int a

modNest :: Int -> Game ()
modNest delta = modExtra (+ delta)

msg :: String -> Game ()
msg s = do
  e <- gets matchExtra
  logMsg $ replicate e ' ' ++ s

minscore :: Integer -> Integer -> State -> Move -> Game Integer
minscore alpha beta st m = do
  Match {..} <- get
  msg $ "minscore: st=" ++ (intercalate ", " $ map prettyPrint st) ++
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
          else go alp bet' as
  modNest 1
  retval <- go alpha beta acts
  modNest (-1)
  msg $ "minscore returns " ++ show retval
  return retval

maxscore :: Integer -> Integer -> State -> Game Integer
maxscore alpha beta st = do
  Match {..} <- get
  msg $ "maxscore: st=" ++ (intercalate ", " $ map prettyPrint st)
  retval <- if isTerminal matchDB st
            then return $ goal matchDB st matchRole
            else do
              modNest 1
              let acts = legal matchDB st matchRole
                  go alp _bet [] = return alp
                  go alp bet (a:as) = do
                    s <- minscore alp bet st a
                    let alp' = alp `max` s
                    if alp' >= bet
                      then return bet
                      else go alp' bet as
              res <- go alpha beta acts
              modNest (-1)
              return res
  msg $ "maxscore returns " ++ show retval
  return retval

bestMove :: State -> Game ()
bestMove st0 = do
  Match {..} <- get
  let as = legal matchDB st0 matchRole
  setBest $ head as
  vs <- forM as (minscore 0 100 st0)
  let avs = zip as vs
      bestv = maximum vs
      possas = map fst $ filter ((== bestv) . snd) avs
  msg $ "Moves and values: " ++ show avs
  msg $ "Possible moves: " ++ show possas
  idx <- getRandomR (0, length possas-1)
  setBest $ possas !! idx

alphaBetaPlayer :: Player Int
alphaBetaPlayer = def { initExtra = const 0
                      , handlePlay = basicPlay bestMove }
