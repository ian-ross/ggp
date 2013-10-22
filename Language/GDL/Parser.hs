{-# LANGUAGE DeriveDataTypeable #-}
module Language.GDL.Parser
       ( parse, parseMaybe
       , parseQuery, parseTerm, parseSexp
       , sexpToTerm, sexpsToDatabase
       ) where

import Control.Applicative ((<$>), (<*), (*>))
import Data.List (foldl')
import Text.Parsec.String
import Text.Parsec hiding (parse)
import qualified Text.Parsec as P

import Language.GDL.Syntax


-- | Parse logic database from a string.
parse :: String -> Either ParseError Database
parse s = case parseSexp s of
  Left e -> Left e
  Right sexps -> Right $ sexpsToDatabase sexps

-- | A variant of 'parse' that returns 'Nothing' if the parse fails.
parseMaybe :: String -> Maybe Database
parseMaybe s = either (const Nothing) Just $ parse s

-- | Parse a single query from a string.
parseQuery :: String -> Maybe Query
parseQuery s = case parseSexp s of
  Right [sexp] -> Just $ sexpToQuery sexp
  _            -> Nothing

-- | Parse a single term from a string (used for GGP protocol).
parseTerm :: String -> Maybe Term
parseTerm s = case parseSexp s of
  Right [sexp] -> Just $ sexpToTerm sexp
  _            -> Nothing

-- | Convert a single S-exp to a term.
sexpToTerm :: Sexp -> Term
sexpToTerm (SAtom "_") = Wild
sexpToTerm (SAtom ('?':s)) = Var s
sexpToTerm (SAtom ('$':s)) = AntiVar s
sexpToTerm (SAtom s) = Atom s
sexpToTerm (SList ss) = Compound $ map sexpToTerm ss

-- | Convert a list of S-exps to a logic database.
sexpsToDatabase :: [Sexp] -> Database
sexpsToDatabase = reverse . foldl' (\db s -> convert s : db) []
  where convert :: Sexp -> Clause
        convert (SList (SAtom "<=" : ss)) = case ss of
          [h] -> (sexpToTerm h, Pass)
          [h, t] -> (sexpToTerm h, sexpToQuery t)
          (h:ts) -> (sexpToTerm h, Conjunction $ map sexpToQuery ts)
        convert s = (sexpToTerm s, Pass)

sexpToQuery :: Sexp -> Query
sexpToQuery (SList [SAtom "not", t]) = Negation $ Query $ sexpToTerm t
sexpToQuery (SList [SAtom "distinct", t1, t2]) =
  Distinct (sexpToTerm t1) (sexpToTerm t2)
sexpToQuery (SList (SAtom "and" : ts)) =
  Conjunction $ map sexpToQuery ts
sexpToQuery (SList []) = Pass
sexpToQuery t = Query $ sexpToTerm t

-- | Parse S-Expressions from a String.  If the parse was successful,
-- @Right sexps@ is returned; otherwise, @Left (errorMsg, leftover)@
-- is returned.
parseSexp :: String -> Either ParseError [Sexp]
parseSexp = P.parse (whiteSpace *> many sexpParser) ""

-- | A parser for S-Expressions.  Ignoring whitespace, we follow the
-- following EBNF:
--
-- SEXP           ::= '(' ATOM* ')' | ATOM
-- ATOM           ::= '"' ESCAPED_STRING* '"' | [^ \t\n()]+
-- ESCAPED_STRING ::= ...
--
sexpParser :: Parser Sexp
sexpParser = choice [ list <?> "list", atom <?> "atom" ] where
  list = SList <$> (char '(' *> whiteSpace *>
                    many sexpParser <* char ')') <* whiteSpace
  atom = SAtom . unescape <$> (choice [str, anything]) <* whiteSpace
  str = char '"' *> many (noneOf "\"") <* char '"'
  anything = many1 (noneOf " \t\n()")

-- | A parser for conventional ASCII whitespace and ";" line comments.
whiteSpace :: Parser ()
whiteSpace = many space >> many comment >> return ()
  where
    comment = char ';' >> many (noneOf "\n") >> many space
