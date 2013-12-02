{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language.GDL.Print
       ( prettyPrint, pretty1, printMach ) where

import Data.List
import qualified Data.ByteString.Char8 as B
import Text.PrettyPrint

import Language.GDL.Syntax

class Pretty a where
  pretty :: a -> Doc

prettyPrint :: Pretty a => a -> String
prettyPrint = render . pretty

pretty1 :: Pretty a => a -> String
pretty1 = intercalate "," . lines . prettyPrint

instance Pretty Term where
  pretty (Atom s) = text (B.unpack s)
  pretty (Var i) = text "?" <> text (B.unpack i)
  pretty (AntiVar i) = text "$" <> text (B.unpack i)
  pretty Wild = text "_"
  pretty (Compound ts) = parens $ hsep $ map pretty ts

instance Pretty Query where
  pretty (Query t) = pretty t
  pretty (And qs) = parens (text "and" <+> (hsep $ map pretty qs))
  pretty (Or qs) = parens (text "or" <+> (hsep $ map pretty qs))
  pretty (Not q) = parens (text "not" <+> pretty q)
  pretty (Distinct t1 t2) = parens (text "distinct" <+> pretty t1 <+> pretty t2)
  pretty Pass = empty

instance Pretty Clause where
  pretty (t, Pass) = pretty t
  pretty (t, q) = parens (text "<=" <+> pretty t <+> pretty q)

instance Pretty [Clause] where
  pretty cs = vcat $ map pretty cs

-- | Pretty print a 'Term' with minimal formatting.  Suitable for
-- machine processing.
printMach :: Term -> String
printMach (Atom s)  = let es = escape (B.unpack s)
                      in if shouldQuote es
                         then ('\"' : es) ++ "\""
                         else es
  where
    shouldQuote es =
      null es
      || find (\c -> (c < 'A' || 'z' < c)
                     && (c < '0' || '9' < c)
                     && not (c `elem` "-_+~<>='/*")) es /= Nothing
printMach (Var i)  = "?" ++ B.unpack i
printMach (AntiVar i)  = "$" ++ B.unpack i
printMach Wild  = "_"
printMach (Compound xs) = makeList (map printMach xs)

-- | Turn @["a", "(b)", "c"]@ into @(a (b) c)@.
makeList :: [String] -> String
makeList xs = ('(' : (intercalate " " xs)) ++ ")"
