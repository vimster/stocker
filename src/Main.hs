module Main where

import Network.HTTP

jsonURL :: String
jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"

-- 1. Perform a basic HTTP get request and return the body
getContent :: String -> IO String
getContent url = simpleHTTP (getRequest url) >>= getResponseBody

addNumbers :: Integer -> Integer -> Integer
addNumbers x y = x + y

-- vorVier :: [Num] -> [Num]
-- vorVier [] = [0]
-- vorVier (x:xs) = filter (<4)

main :: IO ()
main = putStrLn "hello"

