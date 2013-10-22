{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language.GDL.Print
       ( prettyPrint, printMach ) where

import Data.List
import Text.PrettyPrint

import Language.GDL.Syntax

class Pretty a where
  pretty :: a -> Doc

prettyPrint :: Pretty a => a -> String
prettyPrint = render . pretty

instance Pretty Term where
  pretty (Atom s) = text s
  pretty (Var i) = text "?" <> text i
  pretty (AntiVar i) = text "$" <> text i
  pretty Wild = text "_"
  pretty (Compound ts) = parens $ hsep $ map pretty ts

instance Pretty Query where
  pretty (Query t) = pretty t
  pretty (Conjunction qs) = parens (text "and" <+> (hsep $ map pretty qs))
  pretty (Negation q) = parens (text "not" <+> pretty q)
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
printMach (Atom s)  = let es = escape s
                      in if shouldQuote es
                         then ('\"' : es) ++ "\""
                         else es
  where
    shouldQuote es =
      null es
      || find (\c -> (c < 'A' || 'z' < c)
                     && (c < '0' || '9' < c)
                     && not (c `elem` "-_+~<>='/*")) es /= Nothing
printMach (Var i)  = "?" ++ i
printMach (AntiVar i)  = "$" ++ i
printMach Wild  = "_"
printMach (Compound xs) = makeList (map printMach xs)

-- | Turn @["a", "(b)", "c"]@ into @(a (b) c)@.
makeList :: [String] -> String
makeList xs = ('(' : (intercalate " " xs)) ++ ")"
