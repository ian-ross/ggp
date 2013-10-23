{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List (intercalate)
import GGP.Player
import GGP.Utils
import Language.GDL

playRandom :: Maybe [(Role, Move)] -> GGP GGPReply
playRandom _mmoves = do
  Match {..} <- get
  liftIO $ putStrLn $ "State: " ++ prettyPrint matchState
  let moves = legal matchDB matchState matchRole
      nmoves = length moves
  liftIO $ putStrLn $ "Legal moves:\n" ++
    (intercalate "\n" $ map prettyPrint moves)
  idx <- getRandomR (0, nmoves-1)
  let move = moves !! idx
  liftIO $ putStrLn $ "Making move: " ++ prettyPrint move
  return $ Action move

main :: IO ()
main = defaultMain $ def { handlePlay = playRandom }
