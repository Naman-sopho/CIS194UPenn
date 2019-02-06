module Homework2.Homework2 where

import Data.Char
import Homework2.Log

parseMessage :: String -> LogMessage
parseMessage message
        | (message !! 0) == 'E' = LogMessage (Error (getErrorLevel message)) (getTimeStamp 'E' (words message)) (getMessage 'E' (words message))
        | (message !! 0) == 'I' = LogMessage Info (getTimeStamp 'I' (words message)) (getMessage 'I' (words message))
        | (message !! 0) == 'W' = LogMessage Warning (getTimeStamp 'W' (words message)) (getMessage 'W' (words message))


getErrorLevel :: String -> Int
getErrorLevel message = read ((words message) !! 1)

getTimeStamp :: Char -> [String] -> TimeStamp
getTimeStamp messageType message
                                | messageType == 'E' = read (message !! 2)
                                | otherwise = read (message !! 1)

getMessage :: Char -> [String] -> String
getMessage messageType message
                              | messageType == 'E' = unwords (drop 3 message)
                              | otherwise = unwords (drop 2 message)
