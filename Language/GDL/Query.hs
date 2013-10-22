module Language.GDL.Query
       ( query
       , instantiate
       ) where

import qualified Data.Map as M
import Data.STRef
import Control.Monad
import Control.Monad.ST
import qualified Data.Maybe as Maybe

import Language.GDL.Syntax
import Language.GDL.Unify

query :: Database -> Query -> [Substitution] -> [Substitution]
query db query frames = runST $ do
  counter <- newSTRef 0
  qeval' counter db query frames

qeval' _ _ Pass frames = return frames
qeval' counter db (Query struct) frames = fmap concat . mapM applied $ frames
  where applied frame = fmap concat . mapM (apply counter db frame struct) $ db
qeval' counter db (Conjunction conjuncts) frames =
  foldM (flip $ qeval' counter db) frames conjuncts
qeval' counter db (Negation child) frames = do
  frames' <- mapM (\frame -> qeval' counter db child [frame]) frames
  return [frame | (frame, []) <- zip frames frames']

apply counter db frame struct clause = do
  num <- readSTRef counter
  modifySTRef counter (+1)
  apply' counter db frame struct (rewriteClause (show num ++ "#") clause)

apply' counter db frame struct (conclusion, body) =
  case unify frame struct conclusion of
    Just frame' -> qeval' counter db body [frame']
    Nothing     -> return []

rewriteClause :: String -> Clause -> Clause
rewriteClause p (bod, concl) = (rewrite p bod, rewriteQ p concl)

rewriteQ :: String -> Query -> Query
rewriteQ p (Query t) = Query $ rewrite p t
rewriteQ p (Conjunction cs) = Conjunction $ map (rewriteQ p) cs
rewriteQ p (Negation c) = Negation $ rewriteQ p c
rewriteQ p Pass = Pass

rewrite :: String -> Term -> Term
rewrite _ x@(Atom _) = x
rewrite p (Var i) = Var (p ++ i)
rewrite p (AntiVar i) = AntiVar i
rewrite p (Compound cs) = Compound $ map (rewrite p) cs

instantiate :: Substitution -> Query -> Query
instantiate _ Pass = Pass
instantiate sub (Query q) = Query $ inst sub q
instantiate sub (Conjunction cs) = Conjunction $ map (instantiate sub) cs
instantiate sub (Negation c) = Negation $ instantiate sub c

inst :: Substitution -> Term -> Term
inst sub x@(Atom _) = x
inst sub (Var i) = case M.lookup i sub of
  Just v  -> inst sub v
  Nothing -> error $ "Cannot instantiate variable " ++ i
inst sub (AntiVar i) = error $ "Cannot instantiate anti-variable " ++ i
inst sub (Compound cs) = Compound $ map (inst sub) cs
