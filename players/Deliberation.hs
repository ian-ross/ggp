{-# LANGUAGE RecordWildCards #-}
module Deliberation (deliberationPlayer) where

import Data.Function (on)
import Data.List (maximumBy)
import GGP.Player
import GGP.Utils
import Language.GDL

bestMove :: State -> GGP () Move
bestMove st0 = do
  Match {..} <- get
  let as = legal matchDB st0 matchRole
      vs = map (value . doone st0) as
      doone s a = applyMoves matchDB s [(matchRole, a)]
      value st = if isTerminal matchDB st
                 then goal matchDB st matchRole
                 else maximum $ map (value . doone st) $
                      legal matchDB st matchRole
      avs = zip as vs
  liftIO $ putStrLn $ "Moves and values: " ++ show avs
  return $ fst $ maximumBy (compare `on` snd) avs

deliberationPlayer :: Player ()
deliberationPlayer = def { handlePlay = basicPlay bestMove }
