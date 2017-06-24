{-# LANGUAGE OverloadedStrings #-}
module Outer where

import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TMChan
import qualified Control.Exception             as X
import           Control.Monad                 (forever, void)
import qualified Data.ByteString.Char8         as BC8
import           Data.List                     (isPrefixOf)
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import           Outer.Comm                    (GhcMod (..), createPool',
                                                poolio)
import           System.IO                     (Handle, hGetLine, hPutStrLn)
import           System.IO.Error               (isEOFError)
import           System.Socket
import           System.Socket.Family.Inet6
import           System.Socket.Protocol.TCP
import           System.Socket.Type.Stream
import           System.Timeout                (timeout)


--------------------------------------------------------------------------------
talk :: TMChan String -> TMChan String -> IO ()
talk inChan outChan =
  X.bracket
    (socket :: IO (Socket Inet6 Stream TCP))
    (\s -> do
       close s
       putStrLn "Listening socket closed.")
    (\s -> do
       setSocketOption s (ReuseAddress True)
       setSocketOption s (V6Only False)
       bind s (SocketAddressInet6 inet6Any 8080 0 0)
       listen s 5
       putStrLn "Listening socket ready..."
       forever $
         acceptAndHandle s inChan outChan `X.catch`
         \e -> print (e :: SocketException))


--------------------------------------------------------------------------------
timeMilli :: Num a => a -> a
timeMilli n = n * 1000


--------------------------------------------------------------------------------
acceptAndHandle :: Socket Inet6 Stream TCP
                -> TMChan String
                -> TMChan String
                -> IO ()
acceptAndHandle s inChan outChan =
  X.bracket (accept s) (close . fst) acceptAndHandle'
  where
    acceptAndHandle' (sock, _addr) = do
      msg <- timeout (timeMilli 3000) $ consumeMessage sock []
      case msg of
        Nothing -> void $ putStrLn "Error consuming input"
        Just msg' -> do
          cmd <- processCmd (BC8.intercalate "" msg') inChan outChan
          let msg'' = fromMaybe "either timed out or invalid command" cmd
          _ <- sendAll sock msg'' msgNoSignal
          return ()


--------------------------------------------------------------------------------
consumeMessage :: Socket f t p -> [BC8.ByteString] -> IO [BC8.ByteString]
consumeMessage sock acc = do
  msgSize <- getMsgSize sock ""
  case msgSize of
    Nothing -> return $ reverse acc
    Just size' -> do
      msg <- receive sock size' msgNoSignal
      if msg == "\EOT"
        then return $ reverse acc
        else consumeMessage sock (msg : acc)


--------------------------------------------------------------------------------
getMsgSize :: Socket f t p -> BC8.ByteString -> IO (Maybe Int)
getMsgSize sock acc = do
  digit <- receive sock 1 msgNoSignal
  if digit == ":"
    then return $ fst <$> BC8.readInt acc
    else getMsgSize sock (acc <> digit)


--------------------------------------------------------------------------------
processCmd
  :: BC8.ByteString
  -> TMChan String
  -> TMChan String
  -> IO (Maybe BC8.ByteString)
processCmd msg inChan outChan = do
  atomically $ writeTMChan inChan (BC8.unpack msg)
  x <- atomically $ readTMChan outChan
  pure (BC8.pack <$> x)


--------------------------------------------------------------------------------
readUntilOk :: Handle -> [String] -> IO [String]
readUntilOk handle acc = do
  result <- X.try (hGetLine handle) :: IO (Either X.IOException String)
  case result of
    Left err ->
      if isEOFError err
        then return acc
        else readUntilOk handle acc
    Right line ->
      if "OK" `isPrefixOf` line
        then pure (line : acc)
        else readUntilOk handle (line : acc)


--------------------------------------------------------------------------------
ghcModCommunicate :: (GhcMod -> IO ()) -> IO b
ghcModCommunicate action = do
  pool <- createPool'
  forever $ poolio pool action


--------------------------------------------------------------------------------
processForGhcComm :: TMChan String -> TMChan String -> GhcMod -> IO ()
processForGhcComm inChan' outChan' ghc = do
  userInputM <- atomically $ readTMChan inChan'
  case userInputM of
    Nothing -> putStrLn "Channel is closed" >> error "Channel is closed"
    Just userInput -> do
      hPutStrLn (stdinHandle ghc) (trim userInput)
      result <- timeout (timeMilli 10000) $ readUntilOk (stdoutHandle ghc) []
      let output = maybe "No response on stdout" (unlines . reverse) result
      atomically $ writeTMChan outChan' output
  where
    trim :: String -> String
    trim = reverse . dropWhile (== '\n') . reverse


--------------------------------------------------------------------------------
run :: IO ()
run = do
  inChan <- newTMChanIO
  outChan <- newTMChanIO
  _ <- forkIO $ ghcModCommunicate (processForGhcComm inChan outChan)
  talk inChan outChan
  return ()
