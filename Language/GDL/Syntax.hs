{-# LANGUAGE DeriveDataTypeable #-}
module Language.GDL.Syntax
       ( Sexp (..)
       , Identifier, Term (..), Query (..), Clause, Database
       , State, Role, Move
       , escape, unescape
       , sexpToInt, termToInt
       , intToSexp
       , (|+|)
       ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char
import Data.Word
import Data.Data
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.String

data Sexp = SAtom ByteString | SList [Sexp]
          deriving (Eq, Show, Data, Typeable)

instance IsString Sexp where
  fromString = SAtom . B8.pack

type Identifier = ByteString

data Term = Atom ByteString
          | Var Identifier
          | Compound [Term]
          | AntiVar Identifier
          | Wild
          deriving (Eq, Ord, Show, Data, Typeable)

instance IsString Term where
  fromString = Atom . B8.pack

data Query = Query Term
           | And [Query]
           | Or [Query]
           | Not Query
           | Distinct Term Term
           | Pass
           deriving (Eq, Ord, Show, Data, Typeable)

type Clause = (Term, Query)

type Database = M.Map ByteString [Clause]

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
unescape :: ByteString -> ByteString
unescape = B.reverse . B.pack . snd .
           (foldl' unescapeChar (False, [])) . B.unpack
  where
    unescapeChar :: (Bool, [Word8]) -> Word8 -> (Bool, [Word8])
    unescapeChar (False, cs) c = if c == fromIntegral (ord '\\')
                                 then (True, cs)
                                 else (False, c : cs)
    unescapeChar (_, cs) c     = (False, c : cs)

intToSexp :: Int -> Sexp
intToSexp i = fromString $ show i

sexpToInt :: Sexp -> Maybe Int
sexpToInt (SAtom s) = case B8.readInt s of
  Nothing -> Nothing
  Just (i, rest) -> if B.null rest then Just i else Nothing
sexpToInt _ = Nothing

termToInt :: Term -> Maybe Int
termToInt (Atom s) = case B8.readInt s of
  Nothing -> Nothing
  Just (i, rest) -> if B.null rest then Just i else Nothing
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
