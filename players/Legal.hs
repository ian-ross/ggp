{-# LANGUAGE RecordWildCards #-}
module Legal (legalPlayer) where

import GGP.Player
import GGP.Utils
import Language.GDL

firstMove :: State -> GGP () ()
firstMove st = do
  Match {..} <- get
  let moves = legal matchDB st matchRole
  liftIO $ putStrLn $ "LEGAL: " ++ show (head moves)
  setBest $ head moves
  liftIO $ putStrLn $ "LEGAL DONE"

legalPlayer :: Player ()
legalPlayer = def { handlePlay = basicPlay firstMove }
