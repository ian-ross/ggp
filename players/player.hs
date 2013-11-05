module Main where

import GGP.Player
import Legal
import Random
import Deliberation
import Minimax
import AlphaBeta
import DepthLimitedMinimax

main :: IO ()
main = defaultMain $ \pas -> do
  case player pas of
    "legal"                 -> runPlayer legalPlayer
    "random"                -> runPlayer randomPlayer
    "deliberation"          -> runPlayer deliberationPlayer
    "minimax"               -> runPlayer minimaxPlayer
    "alphabeta"             -> runPlayer alphaBetaPlayer
    "depth-limited-minimax" -> runPlayer depthLimitedMinimaxPlayer
    _                       -> error "Invalid player type"
