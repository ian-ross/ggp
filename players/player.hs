module Main where

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

main :: IO ()
main = defaultMain $ \pas -> do
  case player pas of
    "legal"                 -> runPlayer legalPlayer
    "random"                -> runPlayer randomPlayer
    "deliberation"          -> runPlayer deliberationPlayer
    "minimax"               -> runPlayer minimaxPlayer
    "alpha-beta"            -> runPlayer alphaBetaPlayer
    "depth-limited-minimax" -> runPlayer depthLimitedMinimaxPlayer
    "mobility-minimax"      -> runPlayer mobilityMinimaxPlayer
    "mobility-alpha-beta"   -> runPlayer mobilityAlphaBetaPlayer
    "goal-alpha-beta"       -> runPlayer goalAlphaBetaPlayer
    _                       -> error "Invalid player type"
