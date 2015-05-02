{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Configurator
import           Data.Configurator.Types     (Config)
import qualified Data.Function               as Func (on)
import           Data.List                   (intercalate, maximumBy)
import qualified Data.Map                    as Map
import           Data.Time
import           Network.HaskellNet.SMTP.SSL as SMTP
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
  in minimumPrice < maximumPrice * 0.8

stockInfoUrl :: Yahoo.QuoteSymbol -> String
stockInfoUrl symbol = "http://finance.yahoo.com/q?s=" ++ symbol

loadConfig :: IO Config
loadConfig = load [Required "appconfig.cfg"]

host :: String
host = "smtp.zoho.com"

sendEmail :: String -> String -> String-> Yahoo.QuoteMap -> IO ()
sendEmail username password receiver quotes = doSMTPSTARTTLS host $ \conn -> do
    authSucceed <- SMTP.authenticate LOGIN username password conn
    if authSucceed
      then print "Authentication error."
      else sendPlainTextMail receiver username subject body conn
  where subject = "Achtung! Gefallene Kurse"
        body    = "test" -- intercalate "\n" $ map (\q -> Yahoo.symbol q ++ ": " ++ stockInfoUrl (Yahoo.symbol q)) quotes

main :: IO ()
main = do
  today <- fmap utctDay getCurrentTime
  config <- loadConfig
  username <- require config "email_user" :: IO String
  password <- require config "email_password" :: IO String
  receiver <- require config "email_receiver" :: IO [String]
  print receiver
  historicalData <- Yahoo.getHistoricalData testQuotes (addDays (-7) today) today
  let dataOfInterest = filterSymbolsOfInterest historicalData
  print dataOfInterest
  sendEmail username password (head receiver) dataOfInterest
