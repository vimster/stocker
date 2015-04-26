{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Time.Calendar   as T (Day (..), fromGregorian)
import qualified Data.Time.Format     as F (formatTime)
-- import qualified Finance.Quote.Yahoo  as Yahoo
import           Network.HTTP

type QuoteSymbol = String

-- | Float is not an fully appropriate currency type, beware. Exported.
type QuoteCurrency = Float

-- | HistoricalQuote reflects the row form of a yahoo historical quote:
-- Date,Open,High,Low,Close,Volume,Adj Close (taken from the csv itself).
-- Exported.
data HistoricalQuote =
      HistoricalQuote {
        symbol   :: QuoteSymbol,
        date     :: T.Day,
        open     :: QuoteCurrency,
        high     :: QuoteCurrency,
        low      :: QuoteCurrency,
        close    :: QuoteCurrency,
        adjclose :: QuoteCurrency,
        volume   :: Int
        } deriving (Show, Generic)

instance FromJSON HistoricalQuote

baseUrl :: String
baseUrl = "https://query.yahooapis.com/v1/public/yql"

getHistoricalData :: [QuoteSymbol] -> IO [HistoricalQuote]
-- getHistoricalData symbols =

-- 1. Perform a basic HTTP get request and return the body
getContent :: String -> IO String
getContent url = simpleHTTP (getRequest url) >>= getResponseBody

get :: URI -> IO B.ByteString
get uri = do
  let req = Request uri GET [] L.empty
  resp <- browse $ do
    request req
  return $ rspBody $ snd resp

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

addNumbers :: Integer -> Integer -> Integer
addNumbers x y = x + y

-- vorVier :: [Num] -> [Num]
-- vorVier [] = [0]
-- vorVier (x:xs) = filter (<4)

main :: IO ()
main = putStrLn "hello"

