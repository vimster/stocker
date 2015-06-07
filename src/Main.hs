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
import           Data.List.Split
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

testFrom ::  Day
testFrom = fromGregorian 2015 04 04
testTo :: Day
testTo = fromGregorian 2015 04 08
testQuotes :: [String]
testQuotes = ["YHOO"]

maximumPrice :: [Yahoo.HistoricalQuote] -> Float
maximumPrice [] = 0
maximumPrice xs = maximum $ map Yahoo.high xs

minimumPrice :: [Yahoo.HistoricalQuote] -> Float
minimumPrice = minimum . map Yahoo.low

filterSymbolsOfInterest :: Yahoo.QuoteMap -> Yahoo.QuoteMap
filterSymbolsOfInterest = Map.filter isQuoteOfInterest

isQuoteOfInterest :: [Yahoo.HistoricalQuote] -> Bool
isQuoteOfInterest list =
  let latestData        = maximumBy (compare `Func.on` Yahoo.date) list
      latestLow         = Yahoo.low latestData
      dataWithoutLatest = filter (/= latestData) list
  in latestLow > 3 && latestLow < 800 && latestLow < maximumPrice dataWithoutLatest * 0.88


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

delimiter :: DecodeOptions
delimiter = defaultDecodeOptions {decDelimiter = fromIntegral $ ord '\t'}

testTsv = BS.pack "haus\tmaus\ntor\tpforte\n"

parseTsv :: String -> Map.Map String String
parseTsv content =
  let input  = BS.pack content
      result = decodeWith delimiter HasHeader input :: Either String (V.Vector (String, String))
  in case result of
    Left _ -> Map.empty
    Right v -> V.foldl (\m (a, b) -> Map.insert a b m) Map.empty v


stockInfoUrl :: Yahoo.QuoteSymbol -> String
stockInfoUrl symbol = "http://finance.yahoo.com/q?s=" ++ symbol

loadConfig :: IO Config
loadConfig = load [Required "appconfig.cfg"]

------------------------------------------------------------------------
--  Email
------------------------------------------------------------------------
host :: String
host = "smtp.zoho.com"

headline ::  String
headline = "Folgende Kurse sind in der letzten Woche um mehr als 12% gesunken:\n"

sendEmail :: String -> String -> [String]-> Yahoo.QuoteMap -> Map.Map String String -> IO ()
sendEmail username password receivers quotes symbols = doSMTPSTARTTLS host $ \conn -> do
    authSucceed <- SMTP.authenticate LOGIN username password conn
    if authSucceed
      then mapM_ (\r -> sendPlainTextMail r username subject body conn) receivers
      else print "Authentication error."
  where subject        = "Gefallene Kurse"
        body           = T.pack $ (headline++) $ intercalate "\n" $ map quoteText (Map.keys quotes)
        quoteText q    = "[" ++ q ++ "] " ++ sym q ++ " (-" ++ show (diff q) ++ "%, " ++ show (currentPrice q) ++ "):\n" ++ stockInfoUrl q
        list k         = Map.findWithDefault [] k quotes
        currentPrice k = truncate $ Yahoo.close $ last $ list k
        sym k          = Map.findWithDefault "" k symbols
        diff k         = truncate $ 100 - (minimumPrice (list k) * 100 / maximumPrice (list k))


main ::  IO ()
main = do
  filePaths <- map ("symbols/"++) <$> readDir "symbols/"
  contents <- mapM readFile filePaths
  let symbols = parseTsv (head contents)
  putStr "symbol count: "
  print $ Map.size symbols
  yesterday <- addDays (-1) <$> fmap utctDay getCurrentTime
  config <- loadConfig
  username <- require config "email_user" :: IO String
  password <- require config "email_password" :: IO String
  receiver <- require config "email_receiver" :: IO [String]
  historicalData <- Yahoo.getHistoricalData (Map.keys symbols) (addDays (-7) yesterday) yesterday
  let quotesOfInterest = filterSymbolsOfInterest historicalData
  putStr "quotes found: "
  print $ Map.size quotesOfInterest
  unless (Map.null quotesOfInterest) $ sendEmail username password receiver quotesOfInterest symbols
