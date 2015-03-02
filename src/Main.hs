{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.ByteString.Char8              as BSC (pack)
import           Data.Maybe
import qualified Data.Text                          as T (pack, strip)
import           Data.Word                          (Word64)
import           Network.URI
import           Options.Applicative

import           Chevalier.Client
import           Vaultaire.Types
import           System.Nagios.Plugin

data PluginOpts = PluginOpts {
    _broker_host :: String,
    _chevalier_uri :: URI,
    _telemetry_origin :: Origin,
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

    parseChevalierURI = option auto $
           long "chevalier-uri"
        <> short 'c'
        <> metavar "CHEVALIER-URI"
        <> value "tcp://localhost:6283"
        <> showDefault
        <> help "Chevalier reader URI"

    parseTelemetryOrigin = argument auto $
           metavar "TELEMETRY-ORIGIN"
        <> help "Origin Vaultaire telemetry is written to"

    parseCheckOrigin = argument auto $
           metavar "CHECK-ORIGIN"
        <> help "Origin for which to check throughput"

main :: IO ()
main = runNagiosPlugin $ do
    PluginOpts{..} <- liftIO $ execParser helpfulParser
    sources <- getAddresses' _chevalier_uri _telemetry_origin (sourceTags _check_origin)
    error "implement me!"
  where
    sourceTags org = [("origin", org), ("telemetry_msg_type", "writer-count-simple-point")]
