{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module GGP.Utils where

import Data.List (transpose, nub)
import Data.Set (Set)
import qualified Data.Set as S

import Language.GDL

-- | Extract list of roles in database.
roles :: Database -> [Role]
roles = qextract [gdlq|(role ?r)|] (\t -> case t of
                                       [gdlq|(role $r)|] -> Just r
                                       _ -> Nothing)

-- | Extract initial state from database.
initState :: Database -> State
initState db =
  let conv i = ([gdl|(true $i)|], Pass)
  in map conv $ qextract [gdlq|(init ?i)|] (\t -> case t of
                                               [gdlq|(init $i)|] -> Just i
                                               _ -> Nothing) db

-- | Determine legal moves in database for a given role and state.
legal :: Database -> State -> Role -> [Move]
legal db st r =
  qextract [gdlq|(legal $r ?m)|] (\t -> case t of
                                     [gdlq|(legal _ $m)|] -> Just m
                                     _ -> Nothing) (db |+| st)

-- | Determine all feasible moves in a game for a given role using the
-- "input" relation.
feasible :: Database -> Role -> Set Move
feasible db r = S.fromList $ qextract [gdlq|(input $r ?m)|]
                (\t -> case t of
                    [gdlq|(input _ $m)|] -> Just m
                    _ -> Nothing) db

-- | Determine the set of joint legal moves for all roles in a given
-- state.
jointLegal :: Database -> State -> [[Move]]
jointLegal db st = transpose $ map (legal db st) (roles db)

-- TODO
jointLegalIf :: Database -> State -> Role -> Move -> [[Move]]
jointLegalIf _db _st _r _m = []

-- | Is a given state a terminal state?
isTerminal :: Database -> State -> Bool
isTerminal db st = not $ null $ query (db |+| st) [gdlq|terminal|]

-- | Determine goal values for a given state.
goals :: Database -> State -> [(Role, Integer)]
goals db st =
  let db' =  db |+| st
      gs = qextract [gdlq|(goal ?r ?g)|] (\t -> case t of
                                             [gdlq|(goal $r $g)|] -> Just (r, g)
                                             _ -> Nothing) db'
  in map (\(r, g) -> (r, toInt g)) gs
  where toInt = maybe (-1) fromIntegral . termToInt

-- | Determine goal values for a given role in a given state.
goal :: Database -> State -> Role -> Integer
goal db st r =
  let db' = db |+| st
      gs = qextract [gdlq|(goal $r ?g)|] (\t -> case t of
                                             [gdlq|(goal _ $g)|] -> Just g
                                             _ -> Nothing) db'
  in case gs of
    [] -> (-1)
    _ -> maybe (-1) fromIntegral . termToInt $ head gs

-- | Apply moves to give a new state.
applyMoves :: Database -> State -> [(Role, Move)] -> State
applyMoves db st rms =
  let ms = map (\(r, m) -> ([gdl|(does $r $m)|], Pass)) rms
      db' = (db |+| st) |+| ms
      conv t = case t of
        [gdlq|(next $i)|] -> Just ([gdl|(true $i)|], Pass)
        _                 -> Nothing
  in nub $ qextract [gdlq|(next ?i)|] conv db'

-- nextStates :: Database -> State -> [State]
-- nextStateIf :: Database -> State -> [Move] -> State
-- nextStateMoves :: Database -> State -> Role -> M.Map Move [State]
