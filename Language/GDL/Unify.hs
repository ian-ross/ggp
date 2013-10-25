module Language.GDL.Unify
       ( Substitution
       , unify
       ) where

import qualified Data.Map as M

import Language.GDL.Syntax

type Substitution = M.Map Identifier Term

occurs :: Identifier -> Term -> Bool
occurs _ (Atom _) = False
occurs ident (Var identr) = ident == identr
occurs ident (Compound children) = any (occurs ident) children
occurs _ _ = False

extend :: Substitution -> Identifier -> Term -> Maybe Substitution
extend sub ident value = case M.lookup ident sub of
  Just struct -> unify sub struct value
  Nothing     -> extvar value
    where extvar (Var identr) = case M.lookup identr sub of
            Just struct -> unify sub (Var ident) struct
            Nothing -> if ident == identr then Just sub else Just extsub
          extvar struct = if occurs ident struct then Nothing else Just extsub
          extsub = M.insert ident value sub

unify :: Substitution -> Term -> Term -> Maybe Substitution
unify sub (Atom x) (Atom y)
  | x == y    = Just sub
  | otherwise = Nothing
unify sub (Var ident) right = extend sub ident right
unify sub left (Var ident) = extend sub ident left
unify sub (Compound []) (Compound []) = Just sub
unify sub (Compound (x:xs)) (Compound (y:ys)) = case unify sub x y of
  Just sub' -> unify sub' (Compound xs) (Compound ys)
  Nothing   -> Nothing
unify _ _ _ = Nothing
