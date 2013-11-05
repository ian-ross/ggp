{-# LANGUAGE RecordWildCards #-}
module Random (randomPlayer) where

import GGP.Player
import GGP.Utils
import Language.GDL

randomMove :: State -> GGP () Move
randomMove st = do
  Match {..} <- get
  let moves = legal matchDB st matchRole
      nmoves = length moves
  idx <- getRandomR (0, nmoves-1)
  return $ moves !! idx

randomPlayer :: Player ()
randomPlayer = def { handlePlay = basicPlay randomMove }
