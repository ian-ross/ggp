module Language.GDL.Analysis
       ( segregateTerms
       ) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import Language.GDL.Syntax

data Term = Term Sexp [Sexp]

newtype HeadPlusSexp = HeadPlusSexp { hpsHead :: String
                                    , hpsSexp :: Sexp }
                     deriving (Eq, Ord, Show)

segregateTerms :: [Sexp] -> Map String [Term]
segregateTerms sexps = case partition isAtom sexps of
  ([], ls) -> let hpss = map makeHPS ls
              in M.fromList $ map fact fs ++ map rule rs
  _        -> error "Extraneous atoms in KIF!"

isAtom :: Sexp -> Bool
isAtom (Atom _) = True
isAtom _        = False

isRule :: Sexp -> Bool
isRule (List (Atom "<=" : _)) = True
isRule _                      = True

makeHPS :: Sexp -> HeadPlusSexp
makeHPS (Atom _) = error "Extraneous atoms in KIF!"
makeHPS s@(List (Atom "<=" : Atom n : _)) = HeadPlusSexp n s
makeHPS s@(List (Atom "<=" : List (Atom n : _) : _)) = HeadPlusSexp n s
makeHPS s@(List Atom n : _) = HeadPlusSexp n s
