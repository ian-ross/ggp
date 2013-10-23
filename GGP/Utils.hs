{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module GGP.Utils where

import Data.List (transpose, nub)

import Language.GDL

-- | Extract list of roles in database.
roles :: Database -> [Role]
roles = qextract [gdlq|(role ?r)|] (\[gdlq|(role $r)|] -> r)

-- | Extract initial state from database.
initState :: Database -> State
initState db =
  let conv i = ([gdl|(true $i)|], Pass)
  in map conv $ qextract [gdlq|(init ?i)|] (\[gdlq|(init $i)|] -> i) db

-- | Determine legal moves in database for a given role and state.
legal :: Database -> State -> Role -> [Move]
legal db st r =
  qextract [gdlq|(legal $r ?m)|] (\[gdlq|(legal _ $m)|] -> m) (db ++ st)

-- | Determine the set of joint legal moves for all roles in a given
-- state.
jointLegal :: Database -> State -> [[Move]]
jointLegal db st = transpose $ map (legal db st) (roles db)

-- TODO
jointLegalIf :: Database -> State -> Role -> Move -> [[Move]]
jointLegalIf db st r m = []

-- | Is a given state a terminal state?
isTerminal :: Database -> State -> Bool
isTerminal db st = not $ null $ query (db ++ st) [gdlq|terminal|]

-- | Determine goal values for a given state.
goals :: Database -> State -> [(Role, Integer)]
goals db st =
  let db' = db ++ st
      gs = qextract [gdlq|(goal ?r ?g)|] (\[gdlq|(goal $r $g)|] -> (r, g)) db'
  in map (\(r, g) -> (r, toInt g)) gs
  where toInt = maybe (-1) fromIntegral . termToInt

-- | Determine goal values for a given role in a given state.
goal :: Database -> State -> Role -> Integer
goal db st r =
  let db' = db ++ st
      gs = qextract [gdlq|(goal $r ?g)|] (\[gdlq|(goal _ $g)|] -> g) db'
  in maybe (-1) fromIntegral . termToInt $ head gs

-- | Apply moves to give a new state.
applyMoves :: Database -> State -> [(Role, Move)] -> State
applyMoves db st rms =
  let ms = map (\(r, m) -> ([gdl|(does $r $m)|], Pass)) rms
      db' = db ++ st ++ ms
      conv [gdlq|(next $i)|] = ([gdl|(true $i)|], Pass)
  in nub $ qextract [gdlq|(next ?i)|] conv db'

-- nextStates :: Database -> State -> [State]
-- nextStateIf :: Database -> State -> [Move] -> State
-- nextStateMoves :: Database -> State -> Role -> M.Map Move [State]
