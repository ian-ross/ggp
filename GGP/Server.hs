{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (log)
import Data.Default
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Strict as CMTS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Control.Monad.Trans.State.Strict hiding (get, put, gets, modify)
import System.Console.CmdArgs.Implicit hiding (Default, def)

import Network.HTTP.Conduit

import Language.GDL hiding (State)
import qualified Language.GDL as GDL
import GGP.Protocol
import GGP.Utils


data Match a = Match { matchDB :: Database
                     , matchState :: !GDL.State
                     , matchRoles :: [Role]
                     , matchLastMoves :: Maybe [(Role, Move)]
                     , matchStartClock :: Int
                     , matchPlayClock :: Int
                     , matchExtra :: a
                     , matchLogging :: Bool }

type GGP a b = RandT StdGen (StateT (Match a) IO) b

get :: Monad m => RandT g (StateT s m) s
get = lift CMTS.get

put :: Monad m => s -> RandT g (StateT s m) ()
put s = lift (CMTS.put s)

gets :: Monad m => (s -> a) -> RandT g (StateT s m) a
gets f = lift (liftM f CMTS.get)

modify :: Monad m => (s -> s) -> RandT g (StateT s m) ()
modify f = lift (CMTS.modify f)

modExtra :: Monad m => (a -> a) -> RandT g (StateT (Match a) m) ()
modExtra f = modify $ \st -> let !new = f (matchExtra st)
                                 in st { matchExtra = new }

logMsg :: String -> GGP a ()
logMsg msg = do
  logging <- gets matchLogging
  when logging $ liftIO $ putStrLn msg

data PlayerArgs = PlayerArgs { playerHost :: String
                             , playerPort :: Int
                             , playerLog :: Maybe String }
                deriving (Show, Data, Typeable)

data ServerArgs = ServerArgs { rulesFile :: String}
                deriving (Show, Data, Typeable)

defaultPlayers = [PlayerArgs {playerHost = "http://localhost", playerPort = 9147, playerLog = Just "player1.log"}]
defaultServerArgs = ServerArgs { rulesFile = "./hunter.kif" }

main :: IO ()
main = runServer defaultServerArgs defaultPlayers
--main = cmdArgs_ serverArgs >>= runServer

runServer :: ServerArgs -> [PlayerArgs] -> IO ()
runServer args players = do
  putStrLn $ "Running server with " ++ show (length players) ++ " players for game " ++ show (rulesFile args)
  kif <- B.readFile $ rulesFile args
  let rules = case parseSexp kif of
                  Left err -> error $ "Can't parse file:" ++ show err
                  Right r -> r
      sclk = "30" -- start clock
      pclk = "30" -- player clock
      match = "matchid123"
      role = "robot"
      startMsg = encodeSexp $ SList ["start", SAtom match, SAtom role,
                       SList rules, sclk, pclk]
      --playMsg = encodeSexp (SList ["play", SAtom match, moves])
      --stopMsg = encodeSexp (SList ["stop", SAtom match, moves])
  forM_ players $ \player -> do
    initReq <- parseUrl $ playerHost player
    let
        req' = initReq {port = playerPort player}
        -- http://stackoverflow.com/questions/5612145/how-to-easily-get-https-post-response-data/5614946#5614946
        req = urlEncodedBody [ ("", startMsg) ] req'
    putStr $ "req: " ++ show req
    response <- withManager $ httpLbs req
    L.putStr $ responseBody response
    
        

  --let (Right db) = parse kif
  --forkIO $ run (playerPort pas) (handler shutdown (playerLog pas) ps matchInfo p)

