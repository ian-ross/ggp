{-# LANGUAGE RecordWildCards #-}
module Main where

import GGP.Player
import GGP.Utils
import Language.GDL

firstMove :: State -> GGP () Move
firstMove st = do
  Match {..} <- get
  let moves = legal matchDB st matchRole
  return $ head moves

main :: IO ()
main = defaultMain $ def { handlePlay = basicPlay firstMove }
