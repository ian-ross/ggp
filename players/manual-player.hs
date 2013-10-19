{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive
import Data.Sexp
import qualified Language.Sexp.Parser as S
import qualified Language.Sexp.Printer as P
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Util
import System.IO

import GGP.Protocol

handler :: Application
handler req = do
  ereq <- ggpParse req
  case ereq of
    Left err -> bytestring status500 [] err
    Right r -> case r of
      Info     -> ggpReply Available
      Stop _ _ -> do
        liftIO $ putStrLn $ "Request: " ++ show r
        ggpReply Done
      Start _ _ _ _ _ -> do
        liftIO $ putStrLn $ "Request: " ++ show r
        ggpReply Ready
      Play _ _ -> do
        liftIO $ putStrLn $ "Request: " ++ show r
        resp <- liftIO getResp
        let resps = P.printMach resp
        bytestring status200 (respHdrs resps) resps

getResp :: IO Sexp
getResp = do
  putStr $ "? "
  hFlush stdout
  resp <- (foldedCase . mk) <$> getLine
  case S.parse $ BL.fromStrict $ B8.pack resp of
    Right [sexp] -> return sexp
    _            -> getResp

main :: IO ()
main = do
  run 9147 handler
