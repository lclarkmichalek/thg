module HG.Proto
       ( protoVersion
       , reciveConnections
       , cleanupConnections
       , sendMessage
       , receiveConfirmation
       , sendAndCheckMessage
       , receiveMessage
       , confirmMessage
       , confirmFailMessage
       , receiveAndConfirmMessage
       ) where

import Control.Monad (join)
import Data.Maybe (listToMaybe)
import Data.List (isPrefixOf)

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStr, hClose, hPutStrLn,
                  BufferMode(..), Handle)
import System.Random (random, getStdRandom)

-- The version of the HGP that this library implements
protoVersion = "0.1"

-- The maximum number of retries attempted on a FAIL message
maxTries = 2

-- Takes the number of incoming connections expected and the port they
-- are connecting to, and returns an association list of each playerID
-- to the socket handle
reciveConnections :: Integer -> Integer -> IO [(Integer, Handle)]
reciveConnections n p = do
  sock <- listenOn $ PortNumber (fromIntegral p)
  join $ fmap sequence $ recivePlayers' sock 0
    where recivePlayers' :: Socket -> Integer -> IO [IO (Integer, Handle)]
          recivePlayers' sock i
            | i == n = return []
            | otherwise = do
              (handle, host, port) <- accept sock
              writeProtoVersion handle
              let p = return $ (n, handle)
              rest <- recivePlayers' sock (n+1)
              return (p : rest)

-- Closes all connections given
cleanupConnections :: [Handle] -> IO()
cleanupConnections hs = (sequence $ map hClose hs) >> return ()

writeProtoVersion :: Handle -> IO()
writeProtoVersion h = hPutStrLn h protoVersion

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- Sends a message to the given Handle, using the string as the body
-- of the message. Returns the messageID of the message
sendMessage :: Handle -> String -> IO Integer
sendMessage h msg = do
  msgID <- getStdRandom random
  hPutStrLn h ("START " ++ (show msgID))
  hPutStrLn h msg
  hPutStrLn h ("END " ++ (show msgID))
  return msgID

-- Tries to read a confirmation line from the handle, and checks that
-- the messageID is the same as the one given. Does not distinguish
-- between FAIL messages and mismatch message ids
receiveConfirmation :: Handle -> Integer -> IO Bool
receiveConfirmation h msgID = do
  line <- hGetLine h
  return $ (isOKLine line) && ((idFromOKLine line) == (Just msgID))

-- Sends a message and checks for confirmation.
sendAndCheckMessage :: Handle -> String -> IO Bool
sendAndCheckMessage h msg = let
  sacMsg h msg tries
    | tries > maxTries = return False
    | otherwise= do
      msgID <- sendMessage h msg
      line <- hGetLine h
      if isFailLine line
        then sacMsg h msg (tries+1)
        else return $ (isOKLine line) && ((idFromOKLine line) == (Just msgID))
  in sacMsg h msg 0

-- Recieves a message from the handle. Returns Nothing if there was a
-- problem parsing the message's headers, otherwise a tuple of (body, messageID)
receiveMessage :: Handle -> IO (Maybe (String, Integer))
receiveMessage h = do
  fst <- hGetLine h
  if isStartLine fst
    then do
    case idFromStartLine fst of
      Just smid' -> do
        (body, mid) <- receiveMessage' h
        case mid of
          Just mid' -> if smid' == mid'
                       then return $ Just (body, mid')
                       else return Nothing
          Nothing -> return Nothing
      Nothing -> return Nothing
    else receiveMessage h
   where receiveMessage' :: Handle -> IO (String, Maybe Integer)
         receiveMessage' h = do
           line <- hGetLine h
           if isEndLine line
             then return ([], idFromEndLine line)
             else do
             (body, mID) <- receiveMessage' h
             return (line ++ body, mID)

-- Sends a confirmation message with the given message id
confirmMessage :: Handle -> Integer -> IO()
confirmMessage h msgID = hPutStrLn h ("OK " ++ (show msgID))

-- Sends a failure message
confirmFailMessage :: Handle -> IO()
confirmFailMessage h = hPutStrLn h "FAIL"

-- Receives a message via receiveMessage, and then confirms it,
-- returning the message
receiveAndConfirmMessage :: Handle -> IO (Maybe String)
receiveAndConfirmMessage h = do
  msg <- receiveMessage h
  case msg of
    Nothing -> confirmFailMessage h >> return Nothing
    Just (msg', id) -> do
      confirmMessage h id
      return $ Just msg'

isStartLine :: String -> Bool
isStartLine = isPrefixOf "START "

isEndLine :: String -> Bool
isEndLine = isPrefixOf "END "

isOKLine :: String -> Bool
isOKLine = isPrefixOf "OK "

isFailLine :: String -> Bool
isFailLine = (==) "FAIL"

idFromLine :: String -> String -> Maybe Integer
idFromLine start = maybeRead . drop (length start)

idFromEndLine :: String -> Maybe Integer
idFromEndLine = idFromLine "END "

idFromStartLine :: String -> Maybe Integer
idFromStartLine = idFromLine "START "

idFromOKLine :: String -> Maybe Integer
idFromOKLine = idFromLine "OK "