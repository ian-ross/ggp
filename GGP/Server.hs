{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (log)
--import Data.Default
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Strict as CMTS
import qualified Data.ByteString.Char8 as B
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.Trans.State.Strict hiding (get, put, gets, modify)
import System.Console.CmdArgs.Implicit hiding (Default, def)

import Network.HTTP.Conduit

import Language.GDL hiding (State)
import qualified Language.GDL as GDL
--import GGP.Protocol
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
                deriving (Data, Typeable)

instance Show PlayerArgs where
  show p = playerHost p ++ ":" ++ show (playerPort p)

data ServerArgs = ServerArgs { rulesFile :: String}
                deriving (Show, Data, Typeable)

defaultPlayers :: [PlayerArgs]
defaultPlayers = [PlayerArgs {playerHost = "http://localhost", playerPort = 9147, playerLog = Just "player1.log"}]

defaultServerArgs :: ServerArgs
defaultServerArgs = ServerArgs { rulesFile = "./hunter.kif" }

main :: IO ()
main = runServer defaultServerArgs defaultPlayers
--main = cmdArgs_ serverArgs >>= runServer

-- TODO: implement
generateId :: B.ByteString
generateId = "matchid123"

-- TODO: implement
shuffle :: [a] -> IO [a]
shuffle a = return a

runServer :: ServerArgs -> [PlayerArgs] -> IO ()
runServer serverArgs playersCfg = do
  putStrLn $ "Running server with " ++ show (length playersCfg) ++ " players for game " ++ show (rulesFile serverArgs)
  kif <- B.readFile $ rulesFile serverArgs
  rules <- case parseSexp kif of
                  Left err -> error $ "Can't parse file:" ++ show err
                  Right r -> return r
  let
      db = sexpsToDatabase rules
      rs = map (\(Atom a) -> a) $ roles db
      sclk = 30 -- start clock
      pclk = 30 -- player clock
      matchId = generateId
      --playMsg = encodeSexp (SList ["play", SAtom matchId, moves])
      --stopMsg = encodeSexp (SList ["stop", SAtom matchId, moves])

  -- TODO: wait until needed number of players is ready
  let playersReady = playersCfg

  when (length rs > length playersReady) $ error ("Not enough players:" ++
          "got " ++ show (length playersReady) ++ ", needed " ++ show (length rs))

  playersShuffled <- shuffle playersReady

  let players = zip playersShuffled rs
  send_start players matchId rules sclk pclk

-- TODO: forkIO
-- TODO: wait sclk seconds if the player doesn't respond 'ready'
send_start :: [(PlayerArgs, B.ByteString)] -> B.ByteString -> [Sexp] -> Int -> Int -> IO ()
send_start players matchId rules sclk pclk = do
  forM_ players $ \(player, role) -> do
    initReq <- parseUrl $ playerHost player
    let startMsg = encodeSexp $ SList ["start", SAtom matchId, SAtom role, SList rules, intToSexp sclk, intToSexp pclk]
        req      = initReq {port = playerPort player, requestBody = RequestBodyBS startMsg}
        -- http://stackoverflow.com/questions/5612145/how-to-easily-get-https-post-response-data/5614946#5614946
    putStr $ "req: " ++ show req
    response <- withManager $ httpLbs req
    putStr $ "answer from " ++ show player ++ ": "
    L8.putStrLn $ responseBody response


  --let (Right db) = parse kif
  --forkIO $ run (playerPort pas) (handler shutdown (playerLog pas) ps matchInfo p)
