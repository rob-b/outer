{-# LANGUAGE OverloadedStrings #-}
module Outer where

import Control.Monad.Trans.Control (control)
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TMChan
import qualified Control.Exception             as X
import           Control.Monad                 (forever, when)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Maybe
import           Control.Retry                 (RetryPolicyM, constantDelay,
                                                limitRetries, retrying)
import qualified Data.ByteString.Char8         as BC8
import           Data.List                     (isPrefixOf)
import           Data.Maybe                    (isJust, fromMaybe)
import           Data.Monoid                   ((<>))
import           Data.Pool                     (LocalPool, Pool, createPool,
                                                destroyResource, putResource,
                                                takeResource)
import           System.Exit                   (ExitCode)
import           System.IO                     (BufferMode (..), Handle,
                                                hGetLine, hPutStrLn,
                                                hSetBinaryMode, hSetBuffering)
import           System.IO.Error               (isEOFError)
import           System.Process                (ProcessHandle,
                                                StdStream (CreatePipe),
                                                createProcess,
                                                getProcessExitCode, proc,
                                                std_err, std_in, std_out,
                                                terminateProcess)
import           System.Socket
import           System.Socket.Family.Inet6
import           System.Socket.Protocol.TCP
import           System.Socket.Type.Stream
import           System.Timeout                (timeout)


talk :: TMChan String -> IO c
talk inChan =
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
       forever $ acceptAndHandle s inChan `X.catch` \e -> print (e :: SocketException))


timeMilli :: Num a => a -> a
timeMilli n = n * 1000


acceptAndHandle :: Socket Inet6 Stream TCP -> TMChan String -> IO ()
acceptAndHandle s inChan =
  X.bracket
    (accept s)
    (\(p, _addr) -> close p)
    (\(p,_addr) -> do
       msg <- timeout (timeMilli 300000) $ receive p 1024 msgNoSignal
       cmd <- processCmd msg inChan
       let msg' = fromMaybe "either timed out or invalid command" cmd
       sendAll p msg' msgNoSignal
       return ())


processCmd :: Maybe BC8.ByteString -> TMChan String -> IO (Maybe BC8.ByteString)
processCmd msg chan =
  let isKnown = maybe False ("check" `BC8.isPrefixOf`)
  in (if isKnown msg then
     runMaybeT $
       do msg' <- MaybeT $ return msg
          lift . atomically $ writeTMChan chan (BC8.unpack msg')
          return msg'
     else return Nothing)


killGhcMod :: GhcMod -> IO ()
killGhcMod (ActiveGhcMod _ _ processHandle') = terminateProcess processHandle'


--------------------------------------------------------------------------------
mkProcess :: IO (Handle, Handle, Handle, ProcessHandle)
mkProcess = do
  let p =
        createProcess
          (proc "ghc-mod" ["-b", "\n", "legacy-interactive"])
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
  (Just hin,Just hout,Just herr,ph) <- p
  hSetBuffering hin LineBuffering
  hSetBuffering herr LineBuffering
  hSetBuffering hout LineBuffering
  hSetBinaryMode hout False
  return (hin, hout, herr, ph)


--------------------------------------------------------------------------------
data GhcMod = ActiveGhcMod
  { stdoutHandle :: Handle
  , stdinHandle :: Handle
  , processHandle :: ProcessHandle
  }


--------------------------------------------------------------------------------
mkGhcMod :: IO GhcMod
mkGhcMod = do
  (hin,hout,_,ph) <- mkProcess
  return
    ActiveGhcMod
    { stdoutHandle = hout
    , stdinHandle = hin
    , processHandle = ph
    }


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
        then return $ line : acc
        else readUntilOk handle (line : acc)


--------------------------------------------------------------------------------
ghcModCommunicate :: TMChan String -> TMChan String -> IO ()
ghcModCommunicate inChan outChan = do
  pool <- createPool mkGhcMod killGhcMod 1 10 1
  forever $ poolio pool (action inChan outChan)

  where
    def :: RetryPolicyM IO
    def = constantDelay 250000 <> limitRetries 2

    ghcCheck :: Pool GhcMod -> (GhcMod, LocalPool GhcMod) -> IO Bool
    ghcCheck pool (resource,local) = do
      exited' <- isJust <$> exited resource
      when exited' $ destroyResource pool local resource
      return exited'

    poolio :: Pool GhcMod -> (GhcMod -> IO ()) -> IO ()
    poolio pool fn =
      control $
      \runInIO ->
         X.mask $
         \restore -> do
           (resource,local) <- 
             retrying def (\_ pair -> ghcCheck pool pair) (\_ -> takeResource pool)
           ret <- 
             restore (runInIO (fn resource)) `X.onException`
             destroyResource pool local resource
           putResource local resource
           return ret

    action :: TMChan String -> TMChan String -> GhcMod -> IO ()
    action inChan' outChan' ghc = do
      putStrLn "Doing action..."
      userInputM <- atomically $ readTMChan inChan'
      case userInputM of
        Nothing -> print "Channel is closed" >> error "Channel is closed"
        Just userInput -> hPutStrLn (stdinHandle ghc) (trim userInput)
      result <- readUntilOk (stdoutHandle ghc) []
      atomically $ writeTMChan outChan' (unlines $ reverse result)
      where trim = reverse . dropWhile (=='\n') . reverse

    exited :: GhcMod -> IO (Maybe ExitCode)
    exited (ActiveGhcMod _ _ processHandle') = getProcessExitCode processHandle'


run :: IO ()
run = do
  inChan <- newTMChanIO
  outChan <- newTMChanIO
  forkIO $ talk inChan
  forkIO . forever $ consume outChan
  ghcModCommunicate inChan outChan


consume :: TMChan String -> IO ()
consume outChan = do
  msgM <- atomically $ readTMChan outChan
  case msgM of
    Nothing -> error "Cannot consume from closed channel"
    Just result -> putStrLn result
