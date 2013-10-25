{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Function (on)
import Data.List (intercalate, maximumBy)
import GGP.Player
import GGP.Utils
import Language.GDL

playDeliberation :: Maybe [(Role, Move)] -> GGP () GGPReply
playDeliberation _mmoves = do
  Match {..} <- get
  liftIO $ putStrLn $ "State: " ++ prettyPrint matchState
  let moves = legal matchDB matchState matchRole
  liftIO $ putStrLn $ "Legal moves:\n" ++
    (intercalate ", " $ map prettyPrint moves)
  move <- bestMove matchDB matchState matchRole
  liftIO $ putStrLn $ "Making move: " ++ prettyPrint move
  return $ Action move

bestMove :: Database -> State -> Role -> GGP () Move
bestMove db st0 r = do
  let as = legal db st0 r
      vs = map (value . doone st0) as
      doone s a = applyMoves db s [(r, a)]
      value st = if isTerminal db st
                 then goal db st r
                 else maximum $ map (value . doone st) $ legal db st r
      avs = zip as vs
  liftIO $ putStrLn $ "Moves and values: " ++ show avs
  return $ fst $ maximumBy (compare `on` snd) avs

main :: IO ()
main = defaultMain $ def { handlePlay = playDeliberation }
