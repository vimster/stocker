{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Function as Func (on)
import           Data.List     (maximumBy)
import qualified Data.Map      as Map
import           Data.Time
import qualified Yahoofinance  as Yahoo


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

main :: IO ()
main = do
  today <- fmap utctDay getCurrentTime
  historicalData <- Yahoo.getHistoricalData testQuotes (addDays (-7) today) today
  print $ filterSymbolsOfInterest historicalData
