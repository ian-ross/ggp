{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Data.CaseInsensitive
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Util
import System.IO

import GGP.Protocol
import Language.GDL

manualHandler :: Application
manualHandler req = do
  ereq <- ggpParse req
  case ereq of
    Left err -> string status500 [] err
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
        let resps = printMach resp
        string status200 (respHdrs resps) resps

getResp :: IO Term
getResp = do
  putStr $ "? "
  hFlush stdout
  resp <- (foldedCase . mk) <$> getLine
  case parseTerm resp of
    Just s -> return s
    _      -> getResp

main :: IO ()
main = run 9147 manualHandler

ok :: Monad m => String -> m Response
ok rep = string status200 (respHdrs rep) rep

ggpReply :: Monad m => GGPReply -> m Response
ggpReply (Action term) = ok $ printMach term
ggpReply rep = ok $ show rep
