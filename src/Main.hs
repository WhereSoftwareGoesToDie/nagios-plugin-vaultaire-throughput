{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.ByteString.Char8              as BSC (pack)
import qualified Data.Text                          as T (pack, strip)
import           Data.Word                          (Word64)
import           Network.BSD
import           Options.Applicative

import           Chevalier.Client
import           Vaultaire.Types
import           System.Nagios.Plugin

data PluginOpts = PluginOpts {
    _broker_host :: String,
    _chevalier_uri :: String,
    _telemetry_origin :: String,
    _check_origin :: String
}

helpfulParser :: ParserInfo PluginOpts
helpfulParser = info (helper <*> optionsParser) fullDesc

optionsParser :: Parser PluginOpts
optionsParser = PluginOpts <$> parseBroker
                           <*> parseChevalierURI
                           <*> parseTelemetryOrigin
                           <*> parseCheckOrigin
  where
    parseBroker = strOption $
           long "broker-host"
        <> short 'b'
        <> metavar "BROKER-HOST"
        <> value "localhost"
        <> showDefault
        <> help "Vault broker host"

    parseChevalierURI = strOption $
           long "chevalier-uri"
        <> short 'c'
        <> metavar "CHEVALIER-URI"
        <> value "tcp://localhost:6283"
        <> showDefault
        <> help "Chevalier reader URI"

    parseTelemetryOrigin = strArgument $
           metavar "TELEMETRY-ORIGIN"
        <> help "Origin Vaultaire telemetry is written to"

    parseCheckOrigin = strArgument $
           metavar "CHECK-ORIGIN"
        <> help "Origin for which to check throughput"

main :: IO ()
main = runNagiosPlugin $ do
    PluginOpts{..} <- liftIO $ execParser helpfulParser
    error "implement me!"
