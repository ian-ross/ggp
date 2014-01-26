{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
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


type RoleString = B.ByteString
type MyMove = Sexp
data Match = Match {   matchId :: B.ByteString
                     , matchDB :: Database
                     , matchRules :: [Sexp]
                     , matchState :: !GDL.State
                     , matchRoles :: [RoleString]
                     , matchPlayers :: [PlayerArgs]
                     , matchMoves :: [MyMove]
                     , matchStartClock :: Int
                     , matchPlayClock :: Int
                     , matchLogging :: Bool }

type GGP a = StateT Match IO a

get :: Monad m => StateT s m s
get = CMTS.get

gets :: (s -> a) -> StateT s IO a
gets f = liftM f CMTS.get

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = CMTS.modify f

logMsg :: String -> GGP ()
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

  -- TODO: wait until needed number of players is ready
  let playersReady = playersCfg

  playersShuffled <- shuffle playersReady

  let
      db = sexpsToDatabase rules
      st = initState db
      rs = map (\(Atom a) -> a) $ roles db
      --rs = roles db
      sclk = 30 -- start clock
      pclk = 30 -- player clock
      matchId = generateId
      match = Match matchId db rules st rs playersShuffled [SAtom "nil"] sclk pclk True
      --playMsg = encodeSexp (SList ["play", SAtom matchId, moves])
      --stopMsg = encodeSexp (SList ["stop", SAtom matchId, moves])


  when (length rs > length playersReady) $ error ("Not enough players:" ++
          "got " ++ show (length playersReady) ++ ", needed " ++ show (length rs))

  start_resp <- liftIO $! evalStateT send_start match
  putStrLn $ "start response:" ++ show start_resp 
  moves <- liftIO $! evalStateT play match
  putStrLn $ "final moves:" ++ show moves

zipMoves :: [RoleString] -> Sexp -> [(Role, Move)]
zipMoves _  (SAtom "nil") = [] 
zipMoves rs (SList ms) = zip (map (\r -> Atom r) rs) (map sexpToTerm ms)
zipMoves _  _             = [] 

-- return the list of moves, one element - all moves for the one step (SList)
play:: GGP [Sexp]
play = do
  Match {..} <- get
  let

      --TODO: implement
      getLegalMoves :: [MyMove]
      getLegalMoves = undefined

      checkResponseMove :: B.ByteString -> Either MyMove MyMove 
      checkResponseMove moveRaw = move
        where
          legals = getLegalMoves
          move = case parseSexp moveRaw of
                        Left _err -> Left $ head legals
                        Right [m] -> 
                            --TODO: check if m in legal moves
                            Right m
                        Right _ -> Left $ head legals

      playStep :: Int -> GGP ()
      playStep step = do
        Match {..} <- get
        let players = zip matchPlayers matchRoles
            zms = zipMoves matchRoles (head matchMoves)
            newState = applyMoves matchDB matchState zms
        stepMoves <- mapM (playStepRole step) players
        modify $ \st  -> st {matchMoves = (SList stepMoves) : matchMoves}

      playStepRole :: Int -> (PlayerArgs, RoleString) -> GGP Sexp
      playStepRole step (player,role) = do
        Match {..} <- get
        initReq <- parseUrl $ playerHost player
        let 
            playMsg = encodeSexp (SList ["play", SAtom matchId, head matchMoves])
            req      = initReq {port = playerPort player, requestBody = RequestBodyBS playMsg}
        liftIO $ putStrLn $ "req: " ++ show req
        response <- withManager $ httpLbs req
        let moveRaw = L8.toStrict $ responseBody response
        liftIO $ putStrLn $ "answer from " ++ show player ++ ": " ++ (B.unpack moveRaw)
        case checkResponseMove moveRaw of
            Left m -> do 
              liftIO $ putStrLn $ "incorrect move " ++ (show moveRaw) ++ "; use legal move instead: " ++ (show m)
              return m
            Right m -> return m

  mapM_ playStep [1..5]

  --gets matchMoves
  Match{..} <- get
  return matchMoves
  

-- TODO: forkIO
-- TODO: wait sclk seconds if the player doesn't respond 'ready'
send_start :: GGP ()
send_start = do
  Match {..} <- get
  let players = zip matchPlayers matchRoles
  forM_ players $ \(player, role) -> liftIO $ do
    initReq <- parseUrl $ playerHost player
    let startMsg = encodeSexp $ SList ["start", SAtom matchId, SAtom role, SList matchRules, intToSexp matchStartClock, intToSexp matchPlayClock]
        req      = initReq {port = playerPort player, requestBody = RequestBodyBS startMsg}
    liftIO $ putStr $ "req: " ++ show req
    response <- withManager $ httpLbs req
    liftIO $ putStr $ "answer from " ++ show player ++ ": "
    L8.putStrLn $ responseBody response


  --let (Right db) = parse kif
  --forkIO $ run (playerPort pas) (handler shutdown (playerLog pas) ps matchInfo p)
