{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Minimax (minimaxPlayer) where

import Control.Monad (when)
import Data.Function (on)
import Data.List (intercalate, maximumBy, delete)
import GGP.Player
import GGP.Utils
import Language.GDL

type Game a = GGP Int a

data MoveType = Max | Min deriving (Show)

modNest :: Int -> Game ()
modNest delta = modify (\m -> m { matchExtra = matchExtra m + delta })

makeMove :: MoveType -> State -> Game Integer
makeMove t st = do
  Match {..} <- get
  logMsg $ (replicate matchExtra ' ') ++
    "makeMove " ++ show t ++ ": st=" ++ (intercalate ", " $ map prettyPrint st)

  let oppRole = head $ delete matchRole $ roles matchDB
      (nextt, findRes, role) = case t of
        Max -> (Min, maximum, matchRole)
        Min -> (Max, minimum, oppRole)
      as = legal matchDB st role
      poss = map (\a -> applyMoves matchDB st [(role, a)]) as

  retval <- if isTerminal matchDB st
            then return $ goal matchDB st matchRole
            else do
              modNest 1
              ss <- mapM (makeMove nextt) poss
              modNest (-1)
              when (matchExtra == 0) $ logMsg $ "Moves and values: " ++ show ss
              return $ findRes ss

  logMsg $ (replicate matchExtra ' ') ++ "makeMove " ++ show t ++
    " returns " ++ show retval
  return retval

bestMove :: State -> Game ()
bestMove st0 = do
  Match {..} <- get
  let as = legal matchDB st0 matchRole
      poss = map (\a -> applyMoves matchDB st0 [(matchRole, a)]) as
  setBest $ head as
  ss <- mapM (makeMove Min) poss
  setBest $ fst $ maximumBy (compare `on` snd) $ zip as ss

minimaxPlayer :: Player Int
minimaxPlayer = def { initExtra = const (return 0)
                    , handlePlay = basicPlay bestMove }
