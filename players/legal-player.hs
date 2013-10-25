{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List (intercalate)
import GGP.Player
import GGP.Utils
import Language.GDL

playLegal :: Maybe [(Role, Move)] -> GGP () GGPReply
playLegal _mmoves = do
  Match {..} <- get
  liftIO $ putStrLn $ "State: " ++ prettyPrint matchState
  let moves = legal matchDB matchState matchRole
  liftIO $ putStrLn $ "Legal moves:\n" ++
    (intercalate "\n" $ map prettyPrint moves)
  let move = head moves
  liftIO $ putStrLn $ "Making move: " ++ prettyPrint move
  return $ Action move

main :: IO ()
main = defaultMain $ def { handlePlay = playLegal }
