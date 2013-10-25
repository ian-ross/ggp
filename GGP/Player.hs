{-# LANGUAGE DeriveDataTypeable #-}
module GGP.Player
       ( Match (..), Player (..), GGP
       , GGPRequest (..), GGPReply (..)
       , def, defaultMain
       , liftIO, get, put, gets
       , getRandom, getRandoms, getRandomR, getRandomRs ) where

import Prelude hiding (log)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State hiding (get, put, gets)
import qualified Control.Monad.Trans.State as CMTS
import Data.Char
import Data.Default
import Data.IORef
import qualified Data.Map as M
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Util
import System.Console.CmdArgs.Implicit hiding (Default, def)

import Language.GDL hiding (State)
import qualified Language.GDL as GDL
import GGP.Protocol
import GGP.Utils

data Match a = Match { matchDB :: Database
                     , matchState :: GDL.State
                     , matchRole :: Role
                     , matchRoles :: [Role]
                     , matchLastMoves :: Maybe [(Role, Move)]
                     , matchStartClock :: Int
                     , matchPlayClock :: Int
                     , matchExtra :: a
                     } deriving (Show)

type GGP a b = RandT StdGen (StateT (Match a) IO) b

get :: Monad m => RandT g (StateT s m) s
get = lift CMTS.get

put :: Monad m => s -> RandT g (StateT s m) ()
put s = lift (CMTS.put s)

gets :: Monad m => (s -> a) -> RandT g (StateT s m) a
gets f = lift (liftM f CMTS.get)

data Player a = Player { initExtra :: a
                       , handleStart :: GGP a GGPReply
                       , handlePlay :: Maybe [(Role, Move)] -> GGP a GGPReply
                       , handleStop :: Maybe [(Role, Move)] -> GGP a GGPReply
                       }

instance Default a => Default (Player a) where
  def = Player { initExtra = def
               , handleStart = return Ready
               , handleStop = const (return Done)
               , handlePlay = const (return Ready) }

type MatchMap a = M.Map String (IORef (StdGen, Match a))

data PlayerArgs = PlayerArgs { port :: Int, log :: Bool }
                deriving (Show, Data, Typeable)

playerArgs :: Annotate Ann
playerArgs = record PlayerArgs { port = 9147, log = False }
             [ port := 9147 += help "Network port"
             , log := False += help "Message logging"]
             += summary "Generic player interface"
             += program "player"

defaultMain :: Player a -> IO ()
defaultMain player = do
  pas <- cmdArgs_ playerArgs
  matchInfo <- newIORef M.empty
  putStrLn $ "Running on port " ++ show (port pas)
  run (port pas) (handler (log pas) matchInfo player)

handler :: Bool -> IORef (MatchMap a) -> Player a -> Application
handler logging rmatchmap player req = do
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
            Stop matchid moves -> doStop matchid moves player matchmap
            Start matchid role db sclk pclk ->
              doStart matchid role db sclk pclk player rmatchmap
            Play matchid moves -> doPlay matchid moves player matchmap
            Info -> error "Oops"
      when logging $
        liftIO $ putStrLn $ "RESP: " ++ show response
      ggpReply response

doStart :: String -> Term -> Database -> Int -> Int -> Player a
        -> IORef (MatchMap a) -> ResourceT IO GGPReply
doStart matchid role db sclk pclk player rmatchmap = do
  let st = initState db
  gen <- liftIO $ newStdGen
  let extra = initExtra player
      match = Match db st role (roles db) Nothing sclk pclk extra
  matchmap <- liftIO $ readIORef rmatchmap
  rmatch <- liftIO $ newIORef (gen, match)
  liftIO $ writeIORef rmatchmap (M.insert matchid rmatch matchmap)
  let start = handleStart player
  ((resp, gen'), match') <- liftIO $ runStateT (runRandT start gen) match
  liftIO $ writeIORef rmatch (gen', match')
  return resp

doPlay :: String -> Term -> Player a -> MatchMap a -> ResourceT IO GGPReply
doPlay matchid moves player matchmap = do
  let rmatch = matchmap M.! matchid
  (gen, m) <- liftIO $ readIORef rmatch
  let zms = zipMoves (matchRoles m) moves
      st = matchState m
      st' = maybe st (applyMoves (matchDB m) st) zms
      match' = m { matchState = st', matchLastMoves = zms }
      play = handlePlay player zms
  ((resp, gen'), match'') <- liftIO $ runStateT (runRandT play gen) match'
  liftIO $ writeIORef rmatch (gen', match'')
  return resp

doStop :: String -> Term -> Player a -> MatchMap a -> ResourceT IO GGPReply
doStop matchid moves player matchmap = do
  let rmatch = matchmap M.! matchid
  (gen, match) <- liftIO $ readIORef rmatch
  let rs = matchRoles match
      stop = handleStop player (zipMoves rs moves)
  ((resp, gen'), match') <- liftIO $ runStateT (runRandT stop gen) match
  liftIO $ writeIORef rmatch (gen', match')
  return resp

zipMoves :: [Role] -> Term -> Maybe [(Role, Move)]
zipMoves _  (Atom "nil") = Nothing
zipMoves rs (Compound ms) = Just $ zip rs ms
zipMoves _  _             = Nothing

ok :: Monad m => String -> m Response
ok rep = string status200 (respHdrs rep) rep

ggpReply :: Monad m => GGPReply -> m Response
ggpReply (Action term) = ok $ map toLower $ printMach term
ggpReply rep = ok $ map toLower $ show rep
