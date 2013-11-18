module Main where

import Data.List

import GGP.Player
import Legal
import Random
import Deliberation
import Minimax
import AlphaBeta
import DepthLimitedMinimax
import MobilityMinimax
import MobilityAlphaBeta
import GoalAlphaBeta
import MonteCarlo

main :: IO ()
main = defaultMain $ \pas -> do
  case lookup (player pas) availablePlayers of
    Just p  -> p
    _       -> error $ "Invalid player type.\nAvailable types: " ++
                  (intercalate ", " $ map fst availablePlayers)

availablePlayers :: [(String, IO ())]
availablePlayers =
  [ ("legal",                 runPlayer legalPlayer)
  , ("random",                runPlayer randomPlayer)
  , ("deliberation",          runPlayer deliberationPlayer)
  , ("minimax",               runPlayer minimaxPlayer)
  , ("alpha-beta",            runPlayer alphaBetaPlayer)
  , ("depth-limited-minimax", runPlayer depthLimitedMinimaxPlayer)
  , ("mobility-minimax",      runPlayer mobilityMinimaxPlayer)
  , ("mobility-alpha-beta",   runPlayer mobilityAlphaBetaPlayer)
  , ("goal-alpha-beta",       runPlayer goalAlphaBetaPlayer)
  , ("monte-carlo",           runPlayer monteCarloPlayer) ]
