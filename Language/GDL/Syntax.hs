{-# LANGUAGE DeriveDataTypeable #-}
module Language.GDL.Syntax
       ( Sexp (..)
       , Identifier, Term (..), Query (..), Clause, Database
       , State, Role, Move
       , escape, unescape
       , sexpToInt, termToInt
       , (|+|)
       ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Data
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.String

data Sexp = SAtom String | SList [Sexp]
          deriving (Eq, Show, Data, Typeable)

instance IsString Sexp where
  fromString = SAtom

type Identifier = String

data Term = Atom String
          | Var Identifier
          | Compound [Term]
          | AntiVar Identifier
          | Wild
          deriving (Eq, Ord, Show, Data, Typeable)

instance IsString Term where
  fromString = Atom

data Query = Query Term
           | And [Query]
           | Or [Query]
           | Not Query
           | Distinct Term Term
           | Pass
           deriving (Eq, Ord, Show, Data, Typeable)

type Clause = (Term, Query)

type Database = M.Map String [Clause]
--type Database = [Clause]

type Role = Term
type Move = Term
type State = [Clause]


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


sexpToInt :: Sexp -> Maybe Int
sexpToInt (SAtom s) = case B8.readInt $ B8.pack s of
  Nothing -> Nothing
  Just (i, rest) -> if BL.null rest then Just i else Nothing
sexpToInt _ = Nothing

termToInt :: Term -> Maybe Int
termToInt (Atom s) = case B8.readInt $ B8.pack s of
  Nothing -> Nothing
  Just (i, rest) -> if BL.null rest then Just i else Nothing
termToInt _ = Nothing


(|+|) :: Database -> [Clause] -> Database
db |+| sts = db `M.union` stdb
  where stdb = M.fromList $
               map (\cs -> (fst $ head cs, map snd cs)) $
               groupBy ((==) `on` fst) $
               sortBy (compare `on` fst) $
               map convert sts
        convert c = (termName c, c)
        termName (Atom s, _) = s
        termName (Compound ((Atom s) : _), _) = s
        termName _ = error "termName!"
