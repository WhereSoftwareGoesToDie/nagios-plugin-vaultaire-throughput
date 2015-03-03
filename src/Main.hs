{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Monad.Reader
import Data.Maybe
import Data.String
import Data.Word (Word64)
import Network.URI
import Options.Applicative
import Options.Applicative.Types
import qualified Pipes.Prelude as P
import Text.Read (readMaybe)

import Chevalier.Client
import Marquise.Client
import System.Nagios.Plugin
import Vaultaire.Types

data PluginOpts = PluginOpts {
    _brokerHost      :: String,
    _chevalierUri    :: URI,
    _telemetryOrigin :: Origin,
    _checkOrigin     :: Origin
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

    parseChevalierURI = option readURI $
           long "chevalier-uri"
        <> short 'c'
        <> metavar "CHEVALIER-URI"
        <> value (fromJust . parseURI $ "tcp://localhost:6283")
        <> showDefault
        <> help "Chevalier reader URI"

    parseTelemetryOrigin = argument readOrigin $
           metavar "TELEMETRY-ORIGIN"
        <> help "Origin Vaultaire telemetry is written to"

    parseCheckOrigin = argument readOrigin $
           metavar "CHECK-ORIGIN"
        <> help "Origin for which to check throughput"

    readURI :: ReadM URI
    readURI = maybeReadM "URI" parseURI

    readOrigin :: ReadM Origin
    readOrigin = maybeReadM "origin" readMaybe

    maybeReadM :: String -> (String -> Maybe a) -> ReadM a
    maybeReadM name parse = do
        s <- readerAsk
        case parse s of
            Nothing -> readerError $ "Cannot parse " <> name <> ": " <> s
            Just v -> return v

main :: IO ()
main = runNagiosPlugin $ do
    PluginOpts{..} <- liftIO $ execParser helpfulParser
    sources <- getAddresses' _chevalierUri _telemetryOrigin (sourceTags $ show _checkOrigin)
    case sources of
        [(addr,_sd)] -> do
            ts' <- liftIO getCurrentTimeNanoseconds
            let ts = ts' - (10 * 60 * 1000000000)

            r <- liftIO . withReaderConnection _brokerHost $ \conn ->
                runMarquise . P.fold sumPoint 0 id $
                    readSimplePoints NoRetry addr ts ts' _telemetryOrigin conn
            case r of
                Left e  -> addResult Critical . fromString $
                    "Could not get telemetry for origin " <> show _checkOrigin <>
                    " : " <> show e
                Right v -> do
                    if v == 0
                        then addResult Critical . fromString $
                            "Only 0 points for origin " <> show _checkOrigin
                        else addResult OK . fromString $
                            "More than 0 points for origin " <> show _checkOrigin
                    addPerfDatum
                        "points-written"
                        (IntegralValue $ fromIntegral v)
                        NullUnit
                        (Just $ IntegralValue 0)
                        Nothing
                        Nothing
                        Nothing

        l -> addResult Unknown . fromString $ "Found " <> show (length l) <>
            " sources for origin: " <> show _checkOrigin
  where
    sourceTags org =
        [ ("origin", org)
        , ("telemetry_msg_type", "writer-count-simple-point")
        ]

sumPoint :: Word64 -> SimplePoint -> Word64
sumPoint acc (SimplePoint _a _t p) = acc + p
