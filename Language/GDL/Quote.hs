module Language.GDL.Quote
       ( gdl, gdlq
       ) where

import Data.Data
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.AntiQuoter

import Language.GDL.Syntax
import Language.GDL.Parser

gdl :: QuasiQuoter
gdl = QuasiQuoter { quoteExp = gdlToTHExp parseTerm
                  , quotePat  = gdlToTHPat parseTerm
                  , quoteType = error "No type quoter"
                  , quoteDec  = error "No declaration quoter" }

gdlq :: QuasiQuoter
gdlq = QuasiQuoter { quoteExp = gdlToTHExp parseQuery
                   , quotePat  = gdlToTHPat parseQuery
                   , quoteType = error "No type quoter"
                   , quoteDec  = error "No declaration quoter" }

gdlToTHExp :: Data a => (String -> Maybe a) -> String -> Q Exp
gdlToTHExp p s = case p s of
  Nothing -> error $ "Parse failed in GDL quasiquoter"
  Just e  -> dataToExpQ antiE e

gdlToTHPat :: Data a => (String -> Maybe a) -> String -> Q Pat
gdlToTHPat p s = case p s of
  Nothing -> error $ "Parse failed in GDL quasiquoter"
  Just e  -> dataToPatQ antiP e

antiExp :: Term -> Maybe (Q Exp)
antiExp (AntiVar s) = Just . varE $ mkName s
antiExp _           = Nothing

antiE :: AntiQuoter Exp
antiE = antiExp <>> const Nothing

antiPat :: Term -> Maybe (Q Pat)
antiPat (AntiVar s) = Just . varP $ mkName s
antiPat Wild        = Just $ wildP
antiPat _           = Nothing

antiP :: AntiQuoter Pat
antiP = antiPat <>> const Nothing

