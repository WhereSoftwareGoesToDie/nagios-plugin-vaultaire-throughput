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

helpfulParser :: ParserInfo (String, String)
helpfulParser = info (helper <*> optionsParser) fullDesc

optionsParser :: Parser (String, String)
optionsParser = (,) <$> parseBroker <*> parseChevalierURI
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

main :: IO ()
main = runNagiosPlugin $ do
    (broker, chevalier) <- liftIO $ execParser helpfulParser
    error "implement me!"
