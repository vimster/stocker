{-# LANGUAGE OverloadedStrings #-}
module Yahoofinance
( getHistoricalData
, HistoricalQuote (..)
, QuoteMap
, buildHistoricalDataQuery
, QuoteSymbol
) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Function              as Func (on)
import           Data.List                  (groupBy, intercalate, sort)
import           Data.List.Split
import qualified Data.Map                   as Map
import           Data.Time
import           Data.Time
import           Data.Time.Calendar         (Day (..), fromGregorian)
import           Network.HTTP.Conduit       (simpleHttp)
import qualified Network.URI.Encode         as Enc
import           System.Locale              (defaultTimeLocale)

testFrom ::  Day
testFrom = fromGregorian 2015 10 15
testTo ::  Day
testTo = fromGregorian 2015 10 16
testQuotes ::  [QuoteSymbol]
testQuotes = ["AIR.DE"]

today = fmap utctDay getCurrentTime
yesterday = addDays (-1) <$> today

type QuoteSymbol = String
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
        } deriving (Show)

instance Eq HistoricalQuote where
  x == y = symbol x == symbol y && date x == date y

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
getHistoricalData symbols from to
  | len > 500 = Map.unions <$> mapM (\s -> getHistoricalData s from to) (chunk 500 symbols)
  | otherwise = do
    content <- getJSON $ buildHistoricalDataQuery from to symbols
    let parsed = decode content :: Maybe QuoteList
    return $ transformHistoricalData parsed
  where len = length symbols

transformHistoricalData :: Maybe QuoteList -> QuoteMap
transformHistoricalData (Just (QuoteList q)) =
  Map.fromList $ map (\a -> (symbol (head a), sort a)) groupedBySymbol
  where
    groupedBySymbol = groupBy (Func.on (==) symbol) q
transformHistoricalData Nothing = Map.empty

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
