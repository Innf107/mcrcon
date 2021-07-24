{-# LANGUAGE NumDecimals, LambdaCase #-}
module Main where

import Options.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad
import Control.Monad.IO.Class

import System.IO

import Network.RCON.Minecraft


main :: IO ()
main = do
    serverInfo <- execParser (info (optParser <**> helper) mempty)
    runRcon serverInfo 10e6 $
        forever $ prompt >>= sendCommand >>= \case
            Just r  -> liftIO $ TIO.putStrLn r
            Nothing -> liftIO $ putStrLn "TIMEOUT"

prompt :: MonadIO m => m Text
prompt = liftIO $ do
    putStr "> "
    hFlush stdout
    T.pack <$> getLine

optParser :: Parser ServerInfo
optParser = ServerInfo
        <$> strArgument (metavar "HOST")
        <*> argument auto (metavar "PORT")
        <*> strOption (long "password" <> short 'p')


