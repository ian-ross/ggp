{-# LANGUAGE OverloadedStrings #-}
module GGP.Protocol
       ( GGPRequest (..)
       , GGPReply (..)
       , ggpParse
       , respHdrs
       , ggpReply ) where

import Control.Applicative
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.CaseInsensitive
import Data.Sexp
import qualified Language.Sexp.Parser as S
import qualified Language.Sexp.Printer as P
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Util

respHdrs :: ByteString -> ResponseHeaders
respHdrs body = let len = BL.length body
                in [(hContentType, "text/acl"),
                    (hContentLength, BL.toStrict $ B8.pack $ show len)]

data GGPRequest = Info
                | Start { staMatch :: ByteString
                        , staRole :: ByteString
                        , staDesc :: [Sexp]
                        , staStartClock :: Int
                        , staPlayClock :: Int }
                | Play { plaMatch :: ByteString
                       , plaMoves :: Sexp }
                | Stop { stoMatch :: ByteString
                       , stoMoves :: Sexp }
                deriving (Eq, Show)

data GGPReply = Available
              | Ready
              | Action Sexp
              | Done
              deriving (Eq, Show)

ggpParse :: Request -> ResourceT IO (Either ByteString GGPRequest)
ggpParse req = do
  body <- (foldedCase . mk) <$> bodyBytestring req
  case S.parse $ BL.fromStrict body of
    Left _ -> return $ Left "invalid message"
    Right [] -> return $ Left "empty message"
    Right [sexp] -> return $ makeGGPRequest sexp
    Right _ -> return $ Left "invalid message"

makeGGPRequest :: Sexp -> Either ByteString GGPRequest
makeGGPRequest (List ["info"]) = Right Info
makeGGPRequest (List ("start" : Atom match : Atom role : rest)) =
  let lenr = length rest
      desc = take (lenr - 2) rest
      startclock = atomToInt $ (last . init) rest
      playclock = atomToInt $ last rest
  in case (lenr > 2, startclock, playclock) of
    (True, Just sclk, Just pclk) -> Right $ Start match role desc sclk pclk
    _                            -> Left "invalid START message"
makeGGPRequest (List ["play", Atom match, moves]) = Right $ Play match moves
makeGGPRequest (List ["stop", Atom match, moves]) = Right $ Stop match moves
makeGGPRequest _ = Left "unrecognised GGP message type"

ok :: Monad m => ByteString -> m Response
ok rep = bytestring status200 (respHdrs rep) rep

ggpReply :: Monad m => GGPReply -> m Response
ggpReply (Action sexp) = ok $ P.printMach sexp
ggpReply rep = ok $ B8.pack $ show rep

atomToInt :: Sexp -> Maybe Int
atomToInt (Atom s) = case B8.readInt s of
  Nothing -> Nothing
  Just (i, rest) -> if BL.null rest then Just i else Nothing
atomToInt _ = Nothing
