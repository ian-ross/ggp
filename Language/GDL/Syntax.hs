{-# LANGUAGE DeriveDataTypeable #-}
module Language.GDL.Syntax
       ( Sexp (..)
       , Identifier, Term (..), Query (..), Clause, Database
       , escape, unescape
       ) where

import Data.Data
import Data.List
import Data.String

data Sexp = SAtom String | SList [Sexp]
          deriving (Eq, Show, Data, Typeable)

instance IsString Sexp where
  fromString = SAtom

type Identifier = String

data Term = Atom String
          | Var Identifier
          | AntiVar Identifier
          | Compound [Term]
          deriving (Eq, Show, Data, Typeable)

instance IsString Term where
  fromString = Atom

data Query = Query Term
           | Conjunction [Query]
           | Negation Query
           | Pass
           deriving (Eq, Show, Data, Typeable)

type Clause = (Term, Query)

type Database = [Clause]


-- | Escape @"@ and @\@ in the given string.  This needs to be done
-- for double-quoted atoms (e.g. @"\"Hello\", he said"@).
escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar c    = [c]

-- | The inverse of 'escape'.
unescape :: String -> String
unescape = reverse . snd . (foldl' unescapeChar (False, []))
  where
    unescapeChar :: (Bool, [Char]) -> Char -> (Bool, [Char])
    unescapeChar (False, cs) '\\' = (True, cs)
    unescapeChar (_, cs) c        = (False, c : cs)
