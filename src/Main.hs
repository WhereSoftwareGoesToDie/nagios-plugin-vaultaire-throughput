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
import           System.ZMQ4

import           Vaultaire.Types
import           System.Nagios.Plugin

helpfulParser :: ParserInfo String
helpfulParser = info (helper <*> optionsParser) fullDesc

optionsParser :: Parser String
optionsParser = parseBroker
  where
    parseBroker = strOption $
           long "broker"
        <> short 'b'
        <> metavar "BROKER"
        <> value "tcp://localhost:6660"
        <> showDefault
        <> help "Vault broker URI"

main :: IO ()
main = error "implement me!"
