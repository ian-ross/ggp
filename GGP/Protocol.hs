{-# LANGUAGE OverloadedStrings #-}
module GGP.Protocol
       ( GGPRequest (..)
       , GGPReply (..)
       , ggpParse
       , respHdrs ) where

import Control.Applicative
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Util

import Language.GDL

import Debug.Trace

respHdrs :: String -> ResponseHeaders
respHdrs body = let len = length body
                in [(hContentType, "text/acl"),
                    (hContentLength, B.pack $ show len),
                    ("Access-Control-Allow-Origin", "*")]

data GGPRequest = Info
                | Start { reqMatch :: ByteString
                        , staRole :: Role
                        , staDesc :: Database
                        , staStartClock :: Int
                        , staPlayClock :: Int }
                | Play { reqMatch :: ByteString
                       , plaMoves :: Term }
                | Stop { reqMatch :: ByteString
                       , stoMoves :: Term }
                deriving (Eq, Show)

data GGPReply = Available
              | Ready
              | Action Term
              | Done
              deriving (Eq, Show)

ggpParse :: Request -> ResourceT IO (Either String GGPRequest)
ggpParse req = do
  body <- (foldedCase . mk) <$> bodyBytestring req
  case trace ("body:" ++ show body) $ parseSexp body of
    Right [sexp] -> trace ("sexp: " ++ show sexp) $ return $ makeGGPRequest sexp
    _            -> return $ Left "invalid message"

makeGGPRequest :: Sexp -> Either String GGPRequest
makeGGPRequest (SList ["info"]) = Right Info
makeGGPRequest (SList ["start", SAtom match, SAtom role,
                       SList rules, sclk, pclk]) =
  let desc = sexpsToDatabase rules
      startclock = sexpToInt sclk
      playclock = sexpToInt pclk
      trole = Atom role
  in case (startclock, playclock) of
    (Just s, Just p) -> Right $ Start match trole desc s p
    _                -> Left "invalid START message"
makeGGPRequest (SList ["play", SAtom match, moves]) =
  Right $ Play match (sexpToTerm moves)
makeGGPRequest (SList ["stop", SAtom match, moves]) =
  Right $ Stop match (sexpToTerm moves)
makeGGPRequest _ = Left "unrecognised GGP message type"
