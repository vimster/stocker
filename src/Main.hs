{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Time.Calendar         as T (Day (..), fromGregorian)
import qualified Data.Time.Format           as F (formatTime)
import           Network.HTTP.Conduit       (simpleHttp)
-- import qualified Finance.Quote.Yahoo  as Yahoo
import           Control.Applicative
import           Data.List                  (intercalate)
import           Data.Time
import           Network.HTTP
import qualified Network.URI.Encode         as Enc
import           System.Locale              (defaultTimeLocale)

testFrom = fromGregorian 2015 04 04
testTo = fromGregorian 2015 04 08
testQuotes = ["YHOO"]

type QuoteSymbol = String

-- | Float is not an fully appropriate currency type, beware. Exported.
type QuoteCurrency = Float

newtype QuoteList = QuoteList [HistoricalQuote] deriving (Show)

-- | HistoricalQuote reflects the row form of a yahoo historical quote:
-- Date,Open,High,Low,Close,Volume,Adj Close (taken from the csv itself).
-- Exported.
data HistoricalQuote = HistoricalQuote {
        symbol :: QuoteSymbol,
        date   :: String,
        open   :: QuoteCurrency,
        high   :: QuoteCurrency,
        low    :: QuoteCurrency,
        close  :: QuoteCurrency
        -- adjclose :: QuoteCurrency,
        -- volume   :: Int
        } deriving (Show)

instance FromJSON QuoteList where
  parseJSON (Object v) =
    QuoteList <$> (res >>= (.: "quote"))
    where res = (v .: "query") >>= (.: "results")

instance FromJSON HistoricalQuote where
  parseJSON (Object v) = HistoricalQuote <$>
    (v .: "Symbol") <*>
    (v .: "Date") <*>
    (v .: "Open") <*>
    (v .: "High") <*>
    (v .: "Low") <*>
    (v .: "Close")
    -- where res = (v .: "query") >>= (.: "results") >>= (.: "quote")
  -- parseJSON _ = mzero

baseUrl :: String
baseUrl = "http://query.yahooapis.com/v1/public/yql"

-- getHistoricalData :: [QuoteSymbol] -> IO [HistoricalQuote]
-- getHistoricalData symbols =

-- 1. Perform a basic HTTP get request and return the body
getContent :: String -> IO String
getContent url = simpleHTTP (getRequest url) >>= getResponseBody

buildHistoricalDataQuery :: Day -> Day -> [QuoteSymbol] -> String
buildHistoricalDataQuery from to symbols =
  let
    dbFormat = formatTime defaultTimeLocale "%F"
    symbolsFormatted = "'" ++ intercalate "','" symbols ++ "'"
    query = "select * from yahoo.finance.historicaldata \
      \where symbol in (" ++ symbolsFormatted ++ ") and \
      \startDate = '" ++ dbFormat from ++ "' and \
      \endDate = '" ++ dbFormat to ++ "'"
    env = "&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"
  in
    baseUrl ++ "?q=" ++ Enc.encode query ++ "&format=json" ++ env

getJSON :: String -> IO B.ByteString
getJSON = simpleHttp

testJson = BS.pack "{\"query\":{\"results\":{\"quote\":[{\"Symbol\":\"YHOO\",\"Date\":\"2015-04-24\",\"Open\":\"43.73\",\"High\":\"44.71\",\"Low\":\"43.69\",\"Close\":\"44.52\",\"Volume\":\"11267500\",\"Adj_Close\":\"44.52\"}]}}}"

main :: IO ()
main = do
  content <- getJSON $ buildHistoricalDataQuery testFrom testTo testQuotes
  print content
  let historicalData = eitherDecode content :: Either String [HistoricalQuote]
  case historicalData of
    Left error -> putStrLn error
    Right ps -> print ps

