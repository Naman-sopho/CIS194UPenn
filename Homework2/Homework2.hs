module Homework2.Homework2 where

import Data.Char
import Homework2.Log

-- Message parsing
parseMessage :: String -> LogMessage
parseMessage message
        | (message !! 0) == 'E' = LogMessage (Error (getErrorLevel message)) (getTimeStamp 'E' (words message)) (getMessage 'E' (words message))
        | (message !! 0) == 'I' = LogMessage Info (getTimeStamp 'I' (words message)) (getMessage 'I' (words message))
        | (message !! 0) == 'W' = LogMessage Warning (getTimeStamp 'W' (words message)) (getMessage 'W' (words message))
        | otherwise = Unknown message


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

parse :: String -> [LogMessage]
parse input = map parseMessage (lines input)


-- Building message tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown message) node = node
insert message node = case node of
                              Leaf -> Node Leaf message Leaf
                              Node left logMessage right -> if (compareMessage logMessage message)
                                                            then Node left logMessage (insert message right)
                                                            else Node (insert message left) logMessage right

-- Compares timestamps of message at root with the message to be inserted
-- Returns True if message to be inserted is greater
compareMessage :: LogMessage -> LogMessage -> Bool
compareMessage (LogMessage _ rootTimeStamp _) (LogMessage _ newMessageTimeStamp _) = if (rootTimeStamp < newMessageTimeStamp)
                                                                                     then True
                                                                                     else False

-- Builds the message tree from a list of Log messages
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (first:rest) = insert first (build rest)

-- Inorder traversal of the Message tree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = process (inOrder(build list))

-- Returns a list of message strings after filtering errors with severity > 50
process :: [LogMessage] -> [String]
process [] = []
process ((LogMessage (Error severity) _ message) : rest) = if severity > 50
                                                           then [message] ++ process rest
                                                           else process rest

process (LogMessage _ _ _ : rest) = process rest