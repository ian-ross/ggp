{-# LANGUAGE RecordWildCards #-}
module Main where

import GGP.Player
import GGP.Utils
import Language.GDL

playLegal :: Maybe [(Role, Move)] -> GGP GGPReply
playLegal _mmoves = do
  Match {..} <- get
  liftIO $ putStrLn $ "State: " ++ show matchState
  let moves = legal matchDB matchState matchRole
  liftIO $ putStrLn $ "Legal moves: " ++ show moves
  return $ Action $ head moves

main :: IO ()
main = defaultMain $ def { handlePlay = playLegal }
