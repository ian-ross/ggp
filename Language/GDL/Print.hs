module Language.GDL.Print
       ( printHum, printMach ) where

import Data.List

import Language.GDL.Syntax

-- | Maximum length of a list in chars for it to be on a single line.
singleLineCutoff :: Int
singleLineCutoff = 78

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
printMach (Compound xs) = makeList (map printMach xs)

-- | Turn @["a", "(b)", "c"]@ into @(a (b) c)@.
makeList :: [String] -> String
makeList xs = ('(' : (intercalate " " xs)) ++ ")"

-- | Pretty print a 'Sexp' in a human-friendly way.
printHum :: Term -> String
printHum = intercalate "\n" . fst . go
  where
    go :: Term -> ([String], Int)
    go s@(Atom _) = let t = printMach s in ([t], length t)
    go s@(Var _) = let t = printMach s in ([t], length t)
    go s@(AntiVar _) = let t = printMach s in ([t], length t)
    go (Compound ss) =
        let tss = map go ss
            tss' = concat (map fst tss)
        in if all (\ts -> 1 == length (fst ts)) tss
              && sum (map snd tss) + length tss + 2 < singleLineCutoff
           then let t = makeList tss' in ([t], length t)
           else case tss' of
             []   -> error "Internal error"
             [t1] -> let t1' = makeList [t1] in ([t1'], length t1')
             _ -> let t1 = '(' : (head tss')
                      t2 = last tss' ++ ")"
                      tss'' = concat [[t1], map (' ' :)
                                            (tail $ init tss'), [' ' : t2]]
                  in (tss'', maximum (map length tss''))
