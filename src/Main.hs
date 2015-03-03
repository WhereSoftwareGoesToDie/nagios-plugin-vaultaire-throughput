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

-- | The period of time over which to sum written points, in minutes.
checkPeriod :: Word64
checkPeriod = 10

main :: IO ()
main = runNagiosPlugin $ do
    opt@PluginOpts{..} <- liftIO $ execParser helpfulParser
    sources <- getAddresses' _chevalierUri _telemetryOrigin (sourceTags $ show _checkOrigin)
    case sources of
        [] -> addResult Unknown . fromString $ "No sources found for origin: "
            <> show _checkOrigin
        as -> do

            ts' <- liftIO getCurrentTimeNanoseconds
            let ts = ts' - ((fromIntegral checkPeriod) * 60 * 1000000000)

            total <- sumSeries opt ts ts' (fst <$> as)

            case total of
                ([],v) -> do
                    let intervalMsg = " in the last " <> show checkPeriod <> " minutes"
                    if v == 0
                        then addResult Critical . fromString $
                            "No points written for origin " <> show _checkOrigin <> intervalMsg
                        else addResult OK . fromString $
                            "Nonzero points written for origin " <> show _checkOrigin <> intervalMsg
                    addPerfDatum
                        "points-written"
                        (IntegralValue $ fromIntegral v)
                        NullUnit
                        (Just $ IntegralValue 0)
                        Nothing
                        Nothing
                        Nothing
                (es,_) -> addResult Critical . fromString $
                    "Could not get telemetry for origin " <> show _checkOrigin <>
                    ":\n" <> unlines (show <$> es)
  where
    sourceTags org =
        [ ("origin", org)
        , ("telemetry_msg_type", "writer-count-simple-point")
        ]

-- | Sum the time series of simple points from a bunch of addresses.
--
-- Returns a list of any errors encountered and the total.
sumSeries
    :: (MonadIO m)
    => PluginOpts
    -> TimeStamp -- ^ Start of time
    -> TimeStamp -- ^ End of time
    -> [Address] -- ^ Addresses to sum.
    -> m ([MarquiseErrorType], Word64)
sumSeries PluginOpts{..} ts ts' addrs =
    liftIO . withReaderConnection _brokerHost $ \conn ->
        let oneSeries addr = runMarquise . P.fold sumPoint 0 id $
                readSimplePoints NoRetry addr ts ts' _telemetryOrigin conn
        in winnow 0 <$> mapM oneSeries addrs
  where
    winnow :: Num b => b -> [Either a b] -> ([a], b)
    winnow z = work ([],z)
      where work (es,b) [] = (es, b)
            work (es,b) (Left  e:r) = work (e:es,b) r
            work (es,b) (Right v:r) = work (es,b+v) r

sumPoint :: Word64 -> SimplePoint -> Word64
sumPoint acc (SimplePoint _a _t p) = acc + p
