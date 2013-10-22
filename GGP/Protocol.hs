{-# LANGUAGE OverloadedStrings #-}
module GGP.Protocol
       ( GGPRequest (..)
       , GGPReply (..)
       , ggpParse
       , respHdrs ) where

import Control.Applicative
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.CaseInsensitive
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Util

import Language.GDL

respHdrs :: String -> ResponseHeaders
respHdrs body = let len = length body
                in [(hContentType, "text/acl"),
                    (hContentLength, BL.toStrict $ B8.pack $ show len)]

data GGPRequest = Info
                | Start { reqMatch :: String
                        , staRole :: Role
                        , staDesc :: Database
                        , staStartClock :: Int
                        , staPlayClock :: Int }
                | Play { reqMatch :: String
                       , plaMoves :: Term }
                | Stop { reqMatch :: String
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
  case parseSexp $ B8.unpack $ BL.fromStrict body of
    Right [sexp] -> return $ makeGGPRequest sexp
    _            -> return $ Left "invalid message"

makeGGPRequest :: Sexp -> Either String GGPRequest
makeGGPRequest (SList ["info"]) = Right Info
makeGGPRequest (SList ("start" : SAtom match : role : rest)) =
  let lenr = length rest
      desc = sexpsToDatabase $ take (lenr - 2) rest
      startclock = sexpToInt $ (last . init) rest
      playclock = sexpToInt $ last rest
      trole = sexpToTerm role
  in case (lenr > 2, startclock, playclock) of
    (True, Just sclk, Just pclk) -> Right $ Start match trole desc sclk pclk
    _                            -> Left "invalid START message"
makeGGPRequest (SList ["play", SAtom match, moves]) =
  Right $ Play match (sexpToTerm moves)
makeGGPRequest (SList ["stop", SAtom match, moves]) =
  Right $ Stop match (sexpToTerm moves)
makeGGPRequest _ = Left "unrecognised GGP message type"
