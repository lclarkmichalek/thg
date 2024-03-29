module HG.Proto
       ( protoVersion
       , reciveConnections
       , connect
       , cleanupConnections
       , sendMessage
       , receiveMessage

       , Handle
       , withSocketsDo -- Purely for convinience
       ) where

import Prelude hiding (catch)

import Control.Monad (join, void)
import Control.Exception (catch, handle, IOException)

import Text.JSON (encodeStrict, decode,
                  toJSObject, fromJSObject,
                  JSON(..), Result(..),
                  JSObject(..), JSValue(..))

import Network ( listenOn
               , connectTo
               , withSocketsDo
               , accept
               , PortID(..)
               , Socket)
import System.IO (hGetLine, hPutStrLn, hClose, hFlush, Handle)
import System.Random (random, getStdRandom)

-- The version of the HGP that this library implements
protoVersion = "0.2.1"

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
              let p = return (i, handle)
              rest <- recivePlayers' sock (i+1)
              return (p : rest)

connect :: String -> Integer -> IO (Maybe Handle)
connect h p = handle (\e -> let _ = (e :: IOException) in return Nothing) $ do
  h <- connectTo h (PortNumber $ fromIntegral p)
  msg <- (receiveMessage h) :: IO (Maybe (JSObject String))
  case msg of
    Just v -> return (if fromJSObject v == [("version", protoVersion)]
                      then Just h
                      else Nothing)
    Nothing -> return Nothing

-- Closes all connections given
cleanupConnections :: [Handle] -> IO()
cleanupConnections hs = void $ mapM_ hClose hs

writeProtoVersion :: Handle -> IO()
writeProtoVersion h = hPutStrLn h encoded >> hFlush h
  where encoded = encodeStrict (toJSObject [("version", protoVersion)])

-- Sends a message to the given Handle, using the string as the body
-- of the message. Returns the messageID of the message
sendMessage :: Handle -> JSObject JSValue -> IO ()
sendMessage h obj = handle (\e -> let _ = (e :: IOException) in return ())
                    (hPutStrLn h (encodeStrict (showJSON obj)) >> hFlush h)

-- Recieves a message from the handle. Returns Nothing if there was a
-- problem parsing the message's headers, otherwise a tuple of (body, messageID)
receiveMessage :: JSON a => Handle -> IO (Maybe a)
receiveMessage h = do
  raw <- catch ((fmap Just) $ hGetLine h)
      (\e -> let _ = (e :: IOException) in return Nothing)
  case raw of
    Nothing -> return Nothing
    Just raw' -> case decode raw' of
      Ok v -> return $ Just v
      Error _ -> return Nothing
