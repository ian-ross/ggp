module Main where

import Control.DeepSeq
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Language.GDL
import GGP.Utils
import Criterion.Main

instance NFData Term where
  rnf (Atom s) = rnf s
  rnf (Var i) = rnf i
  rnf (Compound ts) = rnf ts
  rnf (AntiVar i) = rnf i
  rnf Wild = ()

main :: IO ()
main = do
  kif <- B.readFile "hunter.kif"
  let (Right db) = parse kif
      s = initState db
      r = head $ roles db
  defaultMain [ bench "legal initstate" $ nf (legal db s) r ]
