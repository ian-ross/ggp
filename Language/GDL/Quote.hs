module Language.GDL.Quote
       ( gdl, gdlq
       ) where

import Data.Data
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.AntiQuoter

import Language.GDL.Syntax
import Language.GDL.Parser

gdl :: QuasiQuoter
gdl = QuasiQuoter { quoteExp = gdlToTHExp
                  , quotePat  = error "No pattern quoter"
                  , quoteType = error "No type quoter"
                  , quoteDec  = error "No declaration quoter" }

gdlq :: QuasiQuoter
gdlq = QuasiQuoter { quoteExp = gdlToTHExp
                   , quotePat  = error "No pattern quoter"
                   , quoteType = error "No type quoter"
                   , quoteDec  = error "No declaration quoter" }

gdlToTHExp :: String -> Q Exp
gdlToTHExp s = case parseTerm s of
  Nothing -> error $ "Parse failed in sexp quasiquoter"
  Just e  -> dataToExpQ anti e

gdlqToTHExp :: String -> Q Exp
gdlqToTHExp s = case parseQuery s of
  Nothing -> error $ "Parse failed in sexp quasiquoter"
  Just e  -> dataToExpQ anti e

antiExp :: Term -> Maybe (Q Exp)
antiExp (AntiVar s) = Just . varE $ mkName s
antiExp _           = Nothing

anti :: AntiQuoter Exp
anti = antiExp <>> const Nothing

