{-# LANGUAGE RecordWildCards #-}
module Main where

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

main :: IO ()
main = defaultMain $ def { handlePlay = basicPlay randomMove }
