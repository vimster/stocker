{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8  as BS
import           Data.Char
import           Data.Configurator
import           Data.Configurator.Types     (Config)
import           Data.Csv
import qualified Data.Function               as Func (on)
import           Data.List                   (intercalate, maximumBy)
import qualified Data.Map                    as Map
import qualified Data.Text.Lazy              as T
import           Data.Time
import qualified Data.Vector                 as V
import           Network.HaskellNet.SMTP.SSL as SMTP
import           System.Directory            (getCurrentDirectory,
                                              getDirectoryContents)
import           System.Environment
import           System.FilePath
import           System.IO                   ()
import qualified Yahoofinance                as Yahoo



testFrom :: Day
testFrom = fromGregorian 2015 04 04
testTo :: Day
testTo = fromGregorian 2015 04 08
testQuotes :: [String]
testQuotes = ["YHOO"]

filterSymbolsOfInterest :: Yahoo.QuoteMap -> Yahoo.QuoteMap
filterSymbolsOfInterest = Map.filter isQuoteOfInterest

isQuoteOfInterest :: [Yahoo.HistoricalQuote] -> Bool
isQuoteOfInterest list =
  let maximumPrice = maximum $ map Yahoo.high list
      latestData = maximumBy (compare `Func.on` Yahoo.date) list
      minimumPrice = Yahoo.low latestData
  in minimumPrice < maximumPrice * 1.8


------------------------------------------------------------------------
--  IO
------------------------------------------------------------------------

isRegularFile :: FilePath -> Bool
isRegularFile f = f /= "." && f /= ".." && takeExtension f == ".txt"

-- | read dir
readDir :: String -> IO [FilePath]
readDir path = do
  directory <- getCurrentDirectory
  filter isRegularFile <$> getDirectoryContents (directory ++ "/" ++ path)

parseTsv :: String -> Map.Map String String
parseTsv content =
  let input = BS.pack content
      parsed = decodeWith defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t'} NoHeader input :: Either String (V.Vector (String, String))
  in Map.empty


stockInfoUrl :: Yahoo.QuoteSymbol -> String
stockInfoUrl symbol = "http://finance.yahoo.com/q?s=" ++ symbol

loadConfig :: IO Config
loadConfig = load [Required "appconfig.cfg"]

------------------------------------------------------------------------
--  Email
------------------------------------------------------------------------
host :: String
host = "smtp.zoho.com"

headline :: String
headline = "Folgende Kurse sind in der letzten Woche um mehr als 20% gesunken:\n\n"

sendEmail :: String -> String -> [String]-> Yahoo.QuoteMap -> IO ()
sendEmail username password receivers quotes = doSMTPSTARTTLS host $ \conn -> do
    authSucceed <- SMTP.authenticate LOGIN username password conn
    if authSucceed
      then print "Authentication error."
      else mapM_ (\r -> sendPlainTextMail r username subject body conn) receivers
  where subject = "Gefallene Kurse"
        body    =   T.pack $ (headline++) $ intercalate "\n" $ map (\q -> q ++ ": " ++ stockInfoUrl q) (Map.keys quotes)





main :: IO ()
main = do
  filePaths <- map ("symbols/"++) <$> readDir "symbols/"
  contents <- mapM readFile filePaths
  yesterday <- addDays (-1) <$> fmap utctDay getCurrentTime
  config <- loadConfig
  username <- require config "email_user" :: IO String
  password <- require config "email_password" :: IO String
  receiver <- require config "email_receiver" :: IO [String]
  historicalData <- Yahoo.getHistoricalData testQuotes (addDays (-7) yesterday) yesterday
  let dataOfInterest = filterSymbolsOfInterest historicalData
  print dataOfInterest
  -- sendEmail username password receiver dataOfInterest
