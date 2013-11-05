{-# LANGUAGE RecordWildCards #-}
module Legal (legalPlayer) where

import GGP.Player
import GGP.Utils
import Language.GDL

firstMove :: State -> GGP () Move
firstMove st = do
  Match {..} <- get
  let moves = legal matchDB st matchRole
  return $ head moves

legalPlayer :: Player ()
legalPlayer = def { handlePlay = basicPlay firstMove }
