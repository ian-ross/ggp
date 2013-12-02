{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module MobilityAlphaBeta (mobilityAlphaBetaPlayer) where

import Control.Monad
import Data.List (delete)
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

minscore :: Int -> Integer -> Integer -> State -> Move -> Game Integer
minscore level alpha beta st m = do
  Match {..} <- get
  msg $ "maxscore: level=" ++ show level ++
    " alpha=" ++ show alpha ++ " beta=" ++ show beta ++
    " m=" ++ prettyPrint m
  let oppRole = head $ delete matchRole $ roles matchDB
      acts = legal matchDB st oppRole
      go _alp bet [] = return bet
      go alp bet (a:as) = do
        let poss = applyMoves matchDB st [(matchRole, m), (oppRole, a)]
        s <- maxscore (level + 1) alp bet poss
        let bet' = bet `min` s
        if bet' <= alp
          then return alp
          else go alp bet' as
  modNest 1
  retval <- go alpha beta acts
  modNest (-1)
  msg $ "minscore returns " ++ show retval
  return retval

maxscore :: Int -> Integer -> Integer -> State -> Game Integer
maxscore level alpha beta st = do
  Match {..} <- get
  msg $ "maxscore: level=" ++ show level ++
    " alpha=" ++ show alpha ++ " beta=" ++ show beta
  let acts = legal matchDB st matchRole
      nas = length acts
  retval <- if isTerminal matchDB st
            then return $ goal matchDB st matchRole
            else if level >= maxDepth matchExtra
                 then do
                   let res = fromIntegral $ 100 * nas `div` matchNFeasible
                   msg $ "Mobility result: " ++ show res
                   return res
                 else do
                   modNest 1
                   let go alp _bet [] = return alp
                       go alp bet (a:as) = do
                         s <- minscore level alp bet st a
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
  vs <- forM as (minscore 0 0 100 st0)
  let avs = zip as vs
      bestv = maximum vs
      possas = map fst $ filter ((== bestv) . snd) avs
  msg $ "Moves and values: " ++ show avs
  msg $ "Possible moves: " ++ show possas
  idx <- getRandomR (0, length possas-1)
  setBest $ possas !! idx

initEx :: PlayerParams -> IO DLState
initEx ps = return $ case getParam "maxDepth" ps of
  Nothing -> DLState 2 0
  Just d -> DLState (read d :: Int) 0

mobilityAlphaBetaPlayer :: Player DLState
mobilityAlphaBetaPlayer = def { initExtra = initEx
                              , handlePlay = basicPlay bestMove }
