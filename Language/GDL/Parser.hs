{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Language.GDL.Parser
       ( parse, parseMaybe
       , parseQuery, parseTerm, parseSexp
       , sexpToTerm, sexpsToDatabase
       ) where

import Control.Applicative ((<$>), (<*), (*>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl', sortBy, groupBy)
import Data.Function (on)
import qualified Data.Map as M
import Text.Parsec.ByteString
import Text.Parsec hiding (parse)
import qualified Text.Parsec as P

import Language.GDL.Syntax


-- | Parse logic database from a string.
parse :: ByteString -> Either ParseError Database
parse s = case parseSexp s of
  Left e -> Left e
  Right sexps -> Right $ sexpsToDatabase sexps

-- | A variant of 'parse' that returns 'Nothing' if the parse fails.
parseMaybe :: ByteString -> Maybe Database
parseMaybe s = either (const Nothing) Just $ parse s

-- | Parse a single query from a string.
parseQuery :: ByteString -> Maybe Query
parseQuery s = case parseSexp s of
  Right [sexp] -> Just $ sexpToQuery sexp
  _            -> Nothing

-- | Parse a single term from a string (used for GGP protocol).
parseTerm :: ByteString -> Maybe Term
parseTerm s = case parseSexp s of
  Right [sexp] -> Just $ sexpToTerm sexp
  _            -> Nothing

-- | Convert a single S-exp to a term.
sexpToTerm :: Sexp -> Term
sexpToTerm (SAtom "_") = Wild
sexpToTerm (SAtom s) = case B.splitAt 1 s of
  ("?", sub) -> Var sub
  ("$", sub) -> AntiVar sub
  _ -> Atom s
sexpToTerm (SList ss) = Compound $ map sexpToTerm ss

-- | Convert a list of S-exps to a logic database.
sexpsToDatabase :: [Sexp] -> Database
sexpsToDatabase = M.fromList .
                  map (\cs -> (fst $ head cs, map snd cs)) .
                  groupBy ((==) `on` fst) .
                  sortBy (compare `on` fst) . reverse .
                  foldl' (\db s -> convert s : db) []
  where convert :: Sexp -> (ByteString, Clause)
        convert (SList (SAtom "<=" : ss)) = case ss of
          [h] -> (termName h, (sexpToTerm h, Pass))
          [h, t] -> (termName h, (sexpToTerm h, sexpToQuery t))
          (h:ts) -> (termName h, (sexpToTerm h, And $ map sexpToQuery ts))
          [] -> error "Free-standing implication symbol"
        convert s = (termName s, (sexpToTerm s, Pass))
        termName (SAtom s) = s
        termName (SList ((SAtom s) : _)) = s
        termName _ = error "termName!"

sexpToQuery :: Sexp -> Query
sexpToQuery (SList [SAtom "not", t]) = Not $ Query $ sexpToTerm t
sexpToQuery (SList [SAtom "distinct", t1, t2]) =
  Distinct (sexpToTerm t1) (sexpToTerm t2)
sexpToQuery (SList (SAtom "and" : ts)) = And $ map sexpToQuery ts
sexpToQuery (SList (SAtom "or" : ts)) = Or $ map sexpToQuery ts
sexpToQuery (SList []) = Pass
sexpToQuery t = Query $ sexpToTerm t

-- | Parse S-Expressions from a String.  If the parse was successful,
-- @Right sexps@ is returned; otherwise, @Left (errorMsg, leftover)@
-- is returned.
parseSexp :: ByteString -> Either ParseError [Sexp]
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
  atom = SAtom . unescape . B.pack <$> (choice [str, anything]) <* whiteSpace
  str = char '"' *> many (noneOf "\"") <* char '"'
  anything = many1 (noneOf " \t\n()")

-- | A parser for conventional ASCII whitespace and ";" line comments.
whiteSpace :: Parser ()
whiteSpace = many space >> many comment >> return ()
  where
    comment = char ';' >> many (noneOf "\n") >> many space
