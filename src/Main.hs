{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Time.Calendar   as T (Day (..), fromGregorian)
import qualified Data.Time.Format     as F (formatTime)
import           Network.HTTP.Conduit (simpleHttp)
-- import qualified Finance.Quote.Yahoo  as Yahoo
import           Control.Applicative
import           Network.HTTP

type QuoteSymbol = String

-- | Float is not an fully appropriate currency type, beware. Exported.
type QuoteCurrency = Float

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

instance FromJSON HistoricalQuote where
  parseJSON (Object v) = HistoricalQuote <$>
    (res >>= (.: "Symbol")) <*>
    (res >>= (.: "Date")) <*>
    (res >>= (.: "Open")) <*>
    (res >>= (.: "High")) <*>
    (res >>= (.: "Low")) <*>
    (res >>= (.: "Close"))
    where res = v .: "result"

baseUrl :: String
baseUrl = "https://query.yahooapis.com/v1/public/yql"

-- getHistoricalData :: [QuoteSymbol] -> IO [HistoricalQuote]
-- getHistoricalData symbols =

-- 1. Perform a basic HTTP get request and return the body
getContent :: String -> IO String
getContent url = simpleHTTP (getRequest url) >>= getResponseBody

getJSON :: String -> IO B.ByteString
getJSON = simpleHttp

main :: IO ()
main = do
  content <- getJSON baseUrl
  let Just historicalData = decode content :: Maybe [HistoricalQuote]
  putStrLn "hello"

