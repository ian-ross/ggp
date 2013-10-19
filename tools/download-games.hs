{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RankNTypes #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit
import System.Directory

-- Given repository name.
download :: String -> IO ()
download r = do
  -- Download list.
  mgames <- decode <$> simpleHttp (repo r ++ "games/") :: IO (Maybe [String])
  case mgames of
    Nothing -> error "Couldn't get game list!"
    Just games -> forM_ games (getGame r)

-- For each game, given repository and game names.
getGame :: String -> String -> IO ()
getGame r g = do
  putStrLn $ "Getting " ++ g ++ " from repository " ++ r
  let base = repo r ++ "games/" ++ g ++ "/"
      bg = base ++ g
      dg = g ++ "/" ++ g

  -- Make directory.
  createDirectoryIfMissing False g

  -- Download files.
  getFile base (g ++ "/METADATA") $ putStrLn "Couldn't get METADATA file"
  getFile (base ++ "rulesheet.kif") (g ++ "/rulesheet.kif") $ putStrLn "Couldn't get KIF file"
  getFile (base ++ "description.txt") (g ++ "/description.txt") $ return ()
  -- getFile (bg ++ ".kif") (dg ++ ".kif") $ putStrLn "Couldn't get KIF file"
  -- getFile (bg ++ ".xsl") (dg ++ ".xsl") $ return ()
  -- getFile (bg ++ ".txt") (dg ++ ".txt") $ return ()
  -- getFile (bg ++ ".js") (dg ++ ".js") $ return ()

-- Get and write a single file.
getFile :: String -> String -> IO () -> IO ()
getFile url fout failed = do
  md <- try $ simpleHttp url
  case md of
    Left (StatusCodeException _ _ _) -> failed
    Right d -> L.writeFile fout d

-- Repository URL.
repo :: String -> String
repo n = "http://games.ggp.org/" ++ n ++ "/"
