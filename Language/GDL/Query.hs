module Language.GDL.Query
       ( query, qextract
       , instantiate
       ) where

import qualified Data.Map as M
import Data.STRef
import Control.Monad
import Control.Monad.ST

import Debug.Trace

import Language.GDL.Syntax
import Language.GDL.Unify

qextract :: Query -> (Query -> a) -> Database -> [a]
qextract q ex db = map (ex . flip instantiate q) $ query db q

query :: Database -> Query -> [Substitution]
query db q = runST $ do
  counter <- newSTRef (0 :: Integer)
  qeval' counter db q [M.empty]

qeval' :: STRef s Integer -> Database -> Query -> [Substitution]
       -> ST s [Substitution]
qeval' _ _ Pass frames = return frames
qeval' counter db (Query struct) frames = fmap concat . mapM applied $ frames
  where applied frame = fmap concat . mapM (apply counter db frame struct) $ db
qeval' counter db (Conjunction conjuncts) frames =
  foldM (flip $ qeval' counter db) frames conjuncts
qeval' counter db (Negation child) frames = do
  frames' <- mapM (\frame -> qeval' counter db child [frame]) frames
  return [frame | (frame, []) <- zip frames frames']
qeval' _counter _db (Distinct c1 c2) frames = return $ filter different frames
  where different f = inst f c1 /= inst f c2

apply :: STRef s Integer -> Database -> Substitution -> Term -> Clause
      -> ST s [Substitution]
apply counter db frame struct clause = do
  num <- readSTRef counter
  modifySTRef counter (+1)
  apply' counter db frame struct (rewriteClause (show num ++ "#") clause)

apply' :: STRef s Integer -> Database -> Substitution -> Term -> Clause
       -> ST s [Substitution]
apply' counter db frame struct (conclusion, body) =
  case unify frame struct conclusion of
    Just frame' -> qeval' counter db body [frame']
    Nothing     -> return []

rewriteClause :: String -> Clause -> Clause
rewriteClause p (bod, concl) = (rewrite p bod, rewriteQ p concl)

rewriteQ :: String -> Query -> Query
rewriteQ p (Query t) = Query $ rewrite p t
rewriteQ p (Conjunction cs) = Conjunction $ map (rewriteQ p) cs
rewriteQ p (Distinct c1 c2) = Distinct (rewrite p c1) (rewrite p c2)
rewriteQ p (Negation c) = Negation $ rewriteQ p c
rewriteQ _ Pass = Pass

rewrite :: String -> Term -> Term
rewrite _ x@(Atom _) = x
rewrite p (Var i) = Var (p ++ i)
rewrite _ (AntiVar i) = AntiVar i
rewrite p (Compound cs) = Compound $ map (rewrite p) cs

instantiate :: Substitution -> Query -> Query
instantiate _ Pass = Pass
instantiate sub (Query q) = Query $ inst sub q
instantiate sub (Conjunction cs) = Conjunction $ map (instantiate sub) cs
instantiate sub (Negation c) = Negation $ instantiate sub c

inst :: Substitution -> Term -> Term
inst _ x@(Atom _) = x
inst sub (Var i) = case M.lookup i sub of
  Just v  -> inst sub v
  Nothing -> error $ "Cannot instantiate variable " ++ i
inst _ (AntiVar i) = error $ "Cannot instantiate anti-variable " ++ i
inst sub (Compound cs) = Compound $ map (inst sub) cs
