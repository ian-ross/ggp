{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module GGP.Player
       ( Match (..), Player (..), GGP
       , GGPRequest (..), GGPReply (..)
       , PlayerArgs (..), PlayerParams, getParam
       , Default (..), defaultMain, runPlayer, basicPlay
       , liftIO, get, put, gets, modify, modExtra, setBest
       , logMsg
       , getRandom, getRandoms, getRandomR, getRandomRs ) where

import Prelude hiding (log)
import Control.Concurrent
import Control.Concurrent.MSampleVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 (ByteString)
import Control.Monad.Trans.State.Strict hiding (get, put, gets, modify)
import qualified Control.Monad.Trans.State.Strict as CMTS
import Data.Char
import Data.Default
import Data.IORef
import Data.List (intercalate, elemIndex)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Util
import System.Console.CmdArgs.Implicit hiding (Default, def)
import System.Exit

import Language.GDL hiding (State)
import qualified Language.GDL as GDL
import GGP.Protocol
import GGP.Utils

data Match a = Match { matchDB :: Database
                     , matchState :: !GDL.State
                     , matchRole :: Role
                     , matchRoleIdx :: Int
                     , matchRoles :: [Role]
                     , matchNRoles :: Int
                     , matchNFeasible :: Int
                     , matchLastMoves :: Maybe [(Role, Move)]
                     , matchStartClock :: Int
                     , matchPlayClock :: Int
                     , matchCurrentBestMove :: MSampleVar Move
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

setBest :: Move -> GGP a ()
setBest m = do
  sv <- gets matchCurrentBestMove
  liftIO $ writeSV sv m

logMsg :: String -> GGP a ()
logMsg msg = do
  logging <- gets matchLogging
  when logging $ liftIO $ putStrLn msg

data Player a = Player { initExtra :: PlayerParams -> IO a
                       , handleStart :: GGP a ()
                       , handlePlay :: GGP a ()
                       , handleStop :: GGP a ()
                       , postMessage :: GGP a ()
                       }

instance Default a => Default (Player a) where
  def = Player { initExtra = return def
               , handleStart = return ()
               , handleStop = return ()
               , handlePlay = return ()
               , postMessage = return () }

type MatchMap a = M.Map ByteString (IORef (StdGen, Match a))

data PlayerArgs = PlayerArgs { player :: String
                             , port :: Int
                             , log :: Bool
                             , params :: String }
                deriving (Show, Data, Typeable)

type PlayerParams = M.Map String String

getParam :: String -> M.Map String String -> Maybe String
getParam = M.lookup

playerArgs :: Annotate Ann
playerArgs = record PlayerArgs { player = "legal", port = 9147
                               , log = False, params = "" }
             [ player := "legal" += help "Player type"
             , port   := 9147    += help "Network port"
             , log    := False   += help "Message logging"
             , params := ""      += help "Player-specific parameters"]
             += summary "Generic player interface"
             += program "player"

defaultMain :: (PlayerArgs -> IO ()) -> IO ()
defaultMain f = cmdArgs_ playerArgs >>= f

runPlayer :: Player a -> IO ()
runPlayer p = do
  pas <- cmdArgs_ playerArgs
  matchInfo <- newIORef M.empty
  putStrLn $ "Running on port " ++ show (port pas)
  let ps = processParams $ params pas
  putStrLn $ "Parameters: " ++
    (intercalate "," $ map (\(k,v) -> k ++ "=" ++ v) $ M.toList ps)
  shutdown <- newEmptyMVar
  forkIO $ run (port pas) (handler shutdown (log pas) ps matchInfo p)
  takeMVar shutdown

processParams :: String -> PlayerParams
processParams = M.fromList . map (spl . trim) . chunks
  where chunks [] = []
        chunks ps = let (p,r) = span (/= ',') ps
                    in p : chunks (drop 1 r)
        trim = trim1 . trim1
        trim1 = dropWhile isSpace . reverse
        spl p = let (n, v) = span (/= ':') p in (trim n, trim $ drop 1 v)

basicPlay :: (GDL.State -> GGP a ()) -> GGP a ()
basicPlay bestMove = do
  Match {..} <- get
  let moves = legal matchDB matchState matchRole
  liftIO $ putStrLn $ "\nLegal moves: " ++
    (intercalate ", " $ map printMach moves)
  setBest $ head moves
  bestMove matchState

handler :: MVar () -> Bool -> PlayerParams -> IORef (MatchMap a)
        -> Player a -> Application
handler shutdown logging params rmatchmap player req = do
  ereq <- ggpParse req
  when logging $
    liftIO $ putStrLn $ "REQ: " ++ show ereq
  case ereq of
    Left err -> string status500 [] err
    Right r -> do
      response <-
        if r == Info
        then return Available
        else do
          matchmap <- liftIO $ readIORef rmatchmap
          case r of
            Stop matchid moves -> do
              res <- doStop matchid moves player matchmap
              liftIO $ putMVar shutdown ()
              return res
            Start matchid role db sclk pclk ->
              doStart matchid role db sclk pclk logging player rmatchmap params
            Play matchid moves -> doPlay matchid moves player matchmap
            Info -> error "Oops"
      when logging $
        liftIO $ putStrLn $ "RESP: " ++ show response
      ggpReply response

doStart :: ByteString -> Term -> Database -> Int -> Int -> Bool -> Player a
        -> IORef (MatchMap a) -> PlayerParams -> ResourceT IO GGPReply
doStart matchid role db sclk pclk logging player rmatchmap params = do
  let st = initState db
      feas = feasible db role
      nfeas = S.size feas
  gen <- liftIO $ newStdGen
  bestMove <- liftIO $ newEmptySV
  extra <- liftIO $ initExtra player params
  let rs = roles db
      match = Match db st role (fromJust $ elemIndex role rs) rs (length rs)
                    nfeas Nothing sclk pclk bestMove extra logging
  matchmap <- liftIO $ readIORef rmatchmap
  rmatch <- liftIO $ newIORef (gen, match)
  liftIO $ writeIORef rmatchmap (M.insert matchid rmatch matchmap)
  let start = handleStart player
  ((_, gen'), match') <- liftIO $! runStateT (runRandT start gen) match
  liftIO $ writeIORef rmatch (gen', match')
  return Ready

doPlay :: ByteString -> Term -> Player a -> MatchMap a -> ResourceT IO GGPReply
doPlay matchid moves player matchmap = do
  let rmatch = matchmap M.! matchid
  (gen, m) <- liftIO $ readIORef rmatch
  let zms = zipMoves (matchRoles m) moves
      st = matchState m
      st' = maybe st (applyMoves (matchDB m) st) zms
      match' = m { matchState = st', matchLastMoves = zms }
      play = handlePlay player
      (gen', playgen) = split gen
  move <- liftIO $ do
    waitVar <- newEmptyMVar
    killer <- forkIO $ do
      threadDelay $ matchPlayClock m * 1000000 - 500000
      putStrLn "Killing worker: timed out"
      putMVar waitVar True
    worker <- forkIO $ do
      _ <- runStateT (runRandT play playgen) match'
      putMVar waitVar True
    void $ readMVar waitVar
    killThread worker
    killThread killer
    mv <- readSV $ matchCurrentBestMove match'
    _ <- runStateT (runRandT (postMessage player) playgen) match'
    putStrLn $ "Making move: " ++ prettyPrint mv
    writeIORef rmatch (gen', match')
    return mv
  return $ Action move

doStop :: ByteString -> Term -> Player a -> MatchMap a -> ResourceT IO GGPReply
doStop matchid moves player matchmap = do
  let rmatch = matchmap M.! matchid
  (gen, match) <- liftIO $ readIORef rmatch
  let rs = matchRoles match
      match' = match { matchLastMoves = zipMoves rs moves }
      stop = handleStop player
  ((_, gen'), match'') <- liftIO $! runStateT (runRandT stop gen) match'
  liftIO $ writeIORef rmatch (gen', match'')
  return Done

zipMoves :: [Role] -> Term -> Maybe [(Role, Move)]
zipMoves _  (Atom "nil") = Nothing
zipMoves rs (Compound ms) = Just $ zip rs ms
zipMoves _  _             = Nothing

ok :: Monad m => String -> m Response
ok rep = string status200 (respHdrs rep) rep

ggpReply :: Monad m => GGPReply -> m Response
ggpReply (Action term) = ok $ map toLower $ printMach term
ggpReply rep = ok $ map toLower $ show rep
