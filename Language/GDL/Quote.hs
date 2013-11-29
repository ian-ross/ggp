{-# LANGUAGE TemplateHaskell #-}
module Language.GDL.Quote
       ( gdl, gdlq
       ) where

import Data.Data
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.AntiQuoter

import Language.GDL.Syntax
import Language.GDL.Parser


gdl :: QuasiQuoter
gdl = QuasiQuoter { quoteExp = gdlToTHExp (parseTerm . B.pack)
                  , quotePat  = gdlToTHPat (parseTerm . B.pack)
                  , quoteType = error "No type quoter"
                  , quoteDec  = error "No declaration quoter" }

gdlq :: QuasiQuoter
gdlq = QuasiQuoter { quoteExp = gdlToTHExp (parseQuery . B.pack)
                   , quotePat  = gdlToTHPat (parseQuery . B.pack)
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
antiExp (AntiVar s) = Just . varE $ mkName $ B.unpack s
antiExp _           = Nothing

bSExp :: ByteString -> Maybe ExpQ
bSExp x = Just $ appE (varE 'B.pack) $ litE $ StringL $ B.unpack x

antiE :: AntiQuoter Exp
antiE = bSExp <>> antiExp <>> const Nothing

antiPat :: Term -> Maybe (Q Pat)
antiPat (AntiVar s) = Just . varP $ mkName $ B.unpack s
antiPat Wild        = Just $ wildP
antiPat _           = Nothing

bSPat :: ByteString -> Maybe PatQ
bSPat x = Just $ litP $ StringL $ B.unpack x

antiP :: AntiQuoter Pat
antiP = bSPat <>> antiPat <>> const Nothing
