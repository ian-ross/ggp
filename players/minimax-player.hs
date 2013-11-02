{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Main where

import Control.Monad
import Data.Function (on)
import Data.List (intercalate, maximumBy, delete)
import GGP.Player
import GGP.Utils
import Language.GDL

type Game a = GGP Int a

modLevel :: Int -> Game ()
modLevel delta = modify (\m -> m { matchExtra = matchExtra m + delta })

boundedMinimum :: [Integer] -> Integer
boundedMinimum [] = 100
boundedMinimum (0:_) = 0
boundedMinimum (x:xs) = x `min` boundedMinimum xs

boundedMaximum :: [Integer] -> Integer
boundedMaximum [] = 0
boundedMaximum (100:_) = 100
boundedMaximum (x:xs) = x `max` boundedMaximum xs

playMinimax :: Maybe [(Role, Move)] -> Game GGPReply
playMinimax _mmoves = do
  Match {..} <- get
  liftIO $ putStrLn $ "State: " ++
    (intercalate ", " $ map prettyPrint matchState)
  let moves = legal matchDB matchState matchRole
  liftIO $ putStrLn $ "Legal moves: " ++
    (intercalate ", " $ map printMach moves)
  move <- bestMove matchState
  liftIO $ putStrLn $ "Making move: " ++ printMach move
  return $ Action move

minscore :: State -> Move -> Game Integer
minscore st m = do
  Match {..} <- get
  logMsg $ (replicate matchExtra ' ') ++
    "minscore: st=" ++ (intercalate ", " $ map prettyPrint st) ++
    " m=" ++ prettyPrint m
  let oppRole = head $ delete matchRole $ roles matchDB
  let as = legal matchDB st oppRole
      poss = map (\a -> applyMoves matchDB st [(matchRole, m), (oppRole, a)]) as
  modLevel 1
  ss <- mapM maxscore poss
  modLevel (-1)
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
              modLevel 1
              ss <- mapM (minscore st) $ legal matchDB st matchRole
              modLevel (-1)
              return $ boundedMaximum ss
  logMsg $ (replicate matchExtra ' ') ++ "maxscore returns " ++ show retval
  return retval

bestMove :: State -> Game Move
bestMove st0 = do
  Match {..} <- get
  let as = legal matchDB st0 matchRole
  vs <- forM as (minscore st0)
  let avs = zip as vs
  logMsg $ "Moves and values: " ++ show avs
  return $ fst $ maximumBy (compare `on` snd) avs

main :: IO ()
main = defaultMain $ def { initExtra = 0
                         , handlePlay = playMinimax }
