module GGP.Player
       ( Match (..), Player (..), GGP
       , GGPRequest (..), GGPReply (..)
       , def, defaultMain
       , liftIO, get, gets ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import Data.Default
import Data.IORef
import qualified Data.Map as M
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Util

import Language.GDL hiding (State)
import qualified Language.GDL as GDL
import GGP.Protocol
import GGP.Utils

data Match = Match { matchDB :: Database
                   , matchState :: GDL.State
                   , matchRole :: Role
                   , matchRoles :: [Role]
                   , matchLastMoves :: Maybe [(Role, Move)]
                   , matchStartClock :: Int
                   , matchPlayClock :: Int
                   } deriving (Eq, Show)

type GGP a = StateT Match IO a

data Player = Player { handleStart :: GGP GGPReply
                     , handlePlay :: Maybe [(Role, Move)] -> GGP GGPReply
                     , handleStop :: Maybe [(Role, Move)] -> GGP GGPReply
                     }

instance Default Player where
  def = Player { handleStart = return Ready
               , handleStop = const (return Done)
               , handlePlay = const (return Ready) }

type MatchMap = M.Map String (IORef Match)

defaultMain :: Player -> IO ()
defaultMain player = do
  matchInfo <- newIORef M.empty
  run 9147 (handler matchInfo player)

handler :: IORef MatchMap -> Player -> Application
handler rmatchmap player req = do
  ereq <- ggpParse req
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
      ggpReply response

doStart :: String -> Term -> Database -> Int -> Int -> Player -> IORef MatchMap
        -> ResourceT IO GGPReply
doStart matchid role db sclk pclk player rmatchmap = do
  let st = initState db
  let match = Match db st role (roles db) Nothing sclk pclk
  matchmap <- liftIO $ readIORef rmatchmap
  rmatch <- liftIO $ newIORef match
  liftIO $ writeIORef rmatchmap (M.insert matchid rmatch matchmap)
  (resp, match') <- liftIO $ flip runStateT match $ handleStart player
  liftIO $ writeIORef rmatch match'
  return resp

doPlay :: String -> Term -> Player -> MatchMap -> ResourceT IO GGPReply
doPlay matchid moves player matchmap = do
  let rmatch = matchmap M.! matchid
  m <- liftIO $ readIORef rmatch
  let zms = zipMoves (matchRoles m) moves
      st = matchState m
      st' = maybe st (applyMoves (matchDB m) st) zms
      match' = m { matchState = st', matchLastMoves = zms }
  (resp, match'') <- liftIO $ flip runStateT match' $ handlePlay player zms
  liftIO $ writeIORef rmatch match''
  return resp

doStop :: String -> Term -> Player -> MatchMap -> ResourceT IO GGPReply
doStop matchid moves player matchmap = do
  let rmatch = matchmap M.! matchid
  match <- liftIO $ readIORef rmatch
  (resp, match') <- liftIO $ flip runStateT match $ do
    rs <- matchRoles <$> get
    handleStop player $ zipMoves rs moves
  liftIO $ writeIORef rmatch match'
  return resp

zipMoves :: [Role] -> Term -> Maybe [(Role, Move)]
zipMoves _  (Atom "nil") = Nothing
zipMoves rs (Compound ms) = Just $ zip rs ms

ok :: Monad m => String -> m Response
ok rep = string status200 (respHdrs rep) rep

ggpReply :: Monad m => GGPReply -> m Response
ggpReply (Action term) = ok $ printMach term
ggpReply rep = ok $ show rep
