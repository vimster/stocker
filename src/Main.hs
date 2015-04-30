{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Function              as Func (on)
import qualified Data.Time.Calendar         as T (Day (..), fromGregorian)
import qualified Data.Time.Format           as F (formatTime)
import           Network.HTTP.Conduit       (simpleHttp)
-- import qualified Finance.Quote.Yahoo  as Yahoo
import           Control.Applicative
import           Data.List                  (groupBy, intercalate, maximumBy)
import qualified Data.Map                   as Map
import           Data.Time
import           Data.Time.Clock
import           Network.HTTP
import qualified Network.URI.Encode         as Enc
import           System.Locale              (defaultTimeLocale)

testFrom = fromGregorian 2015 04 04
testTo = fromGregorian 2015 04 08
testQuotes = ["YHOO"]

type QuoteSymbol = String
type QuoteCurrency = Float
type QuoteMap = Map.Map QuoteSymbol [HistoricalQuote]

newtype QuoteList = QuoteList [HistoricalQuote] deriving (Show)

data HistoricalQuote = HistoricalQuote {
        symbol :: QuoteSymbol,
        date   :: String,
        open   :: Float,
        high   :: Float,
        low    :: Float,
        close  :: Float
        -- adjclose :: QuoteCurrency,
        -- volume   :: Int
        } deriving (Show, Eq)

instance Ord HistoricalQuote where
  HistoricalQuote {date = d} <= HistoricalQuote {date = d2} = d <= d2

instance FromJSON QuoteList where
  parseJSON (Object v) =
    QuoteList <$> (res >>= (.: "quote"))
    where res = (v .: "query") >>= (.: "results")

toFloat :: String -> Float
toFloat = read

instance FromJSON HistoricalQuote where
  parseJSON (Object v) = HistoricalQuote <$>
    (v .: "Symbol") <*>
    (v .: "Date") <*>
    fmap toFloat (v .: "Open") <*>
    fmap toFloat (v .: "High") <*>
    fmap toFloat (v .: "Low") <*>
    fmap toFloat (v .: "Close")
    -- where res = (v .: "query") >>= (.: "results") >>= (.: "quote")
  -- parseJSON _ = mzero

baseUrl :: String
baseUrl = "http://query.yahooapis.com/v1/public/yql"

getHistoricalData :: [QuoteSymbol] -> Day -> Day -> IO QuoteMap
getHistoricalData symbols from to = do
  content <- getJSON $ buildHistoricalDataQuery from to symbols
  let parsed = decode content :: Maybe QuoteList
  return $ transformHistoricalData parsed

transformHistoricalData :: Maybe QuoteList -> QuoteMap
transformHistoricalData (Just (QuoteList q)) =
  Map.fromList $ map (\a -> (symbol (head a), a)) groupedBySymbol
  where
    groupedBySymbol = groupBy (Func.on (==) symbol) q
transformHistoricalData Nothing = Map.empty


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

filterSymbolsOfInterest :: QuoteMap -> QuoteMap
filterSymbolsOfInterest = Map.filter isQuoteOfInterest

isQuoteOfInterest :: [HistoricalQuote] -> Bool
isQuoteOfInterest list =
  let maximumPrice = maximum $ map high list
      latestData = maximumBy (compare `Func.on` date) list
      minimumPrice = low latestData
  in minimumPrice < maximumPrice * 0.8

testJson = BS.pack "{\"query\":{\"results\":{\"quote\":[{\"Symbol\":\"YHOO\",\"Date\":\"2015-04-24\",\"Open\":\"43.73\",\"High\":\"44.71\",\"Low\":\"43.69\",\"Close\":\"44.52\",\"Volume\":\"11267500\",\"Adj_Close\":\"44.52\"}]}}}"

main :: IO ()
main = do
  today <- fmap utctDay getCurrentTime
  historicalData <- getHistoricalData testQuotes (addDays (-7) today) today
  print $ filterSymbolsOfInterest historicalData
  putStrLn "finished"


testmain :: IO ()
testmain = do
  content <- getJSON $ buildHistoricalDataQuery testFrom testTo testQuotes
  let historicalData = eitherDecode content :: Either String [HistoricalQuote]
  case historicalData of
    Left err -> putStrLn err
    Right ps -> print ps

parseJson :: IO ()
parseJson = do
  let historicalData = eitherDecode testJson :: Either String QuoteList
  print historicalData
