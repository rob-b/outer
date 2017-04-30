{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Debug.Trace
import           Control.Concurrent            (forkFinally, threadDelay)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TMChan
import qualified Control.Exception             as X
import           Control.Monad                 (forever, when, void)
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Except    (runExceptT, throwE)
import           Data.List                     (isPrefixOf)
import           Data.Maybe                    (isNothing)
import           Data.Monoid                   ((<>))
import           Network.Socket
import           System.IO                     (BufferMode (LineBuffering),
                                                Handle, IOMode (ReadWriteMode),
                                                hClose, hGetLine, hPutStrLn,
                                                hSetBinaryMode, hSetBuffering, hIsEOF)
import           System.IO.Error               (isEOFError)
import           System.Process
import           System.Process.Internals


--------------------------------------------------------------------------------
getPid :: ProcessHandle -> IO (Maybe PHANDLE)
getPid ph = withProcessHandle ph unpack
  where
    unpack ph_ =
      case ph_ of
        OpenHandle x -> return $ Just x
        ClosedHandle _ -> return Nothing


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
sleep :: Int -> IO ()
sleep x = threadDelay $ x * 1000


data Comm a = Terminated a | Alive a


--------------------------------------------------------------------------------
readUntilOk :: Handle -> [String] -> IO (Comm [String])
readUntilOk handle acc = do
  result <- X.try (hGetLine handle) :: IO (Either X.IOException String)
  case result of
    Left err ->
      if isEOFError err
        then return (Terminated acc)
        else readUntilOk handle acc
    Right line ->
      if "OK" `isPrefixOf` line
        then return $ Alive (line : acc)
        else readUntilOk handle (line : acc)


--------------------------------------------------------------------------------
ghcmodCommunicate :: TMChan String -> TMChan String -> StateT GhcMod IO ()
ghcmodCommunicate inChan outChan = go'
  where
    nextAction :: GhcMod -> StateT GhcMod IO ()
    nextAction DeadGhcMod = return ()
    nextAction ActiveGhcMod {} = go'

    go' :: StateT GhcMod IO ()
    go' = do
      msgM <- lift $ atomically $ readTMChan inChan
      case msgM of
        Nothing ->
          lift $
          void (putStrLn "Channel is closed; ending communication loop")
        Just msg -> do
          ghcmod <- get
          ghcmod' <- lift $ getGhcProcess ghcmod
          ghcmod'' <-
            lift $
            do hPutStrLn (stdinHandle ghcmod') msg
               results <- readUntilOk (stdoutHandle ghcmod') []
               case results of
                 Terminated results' -> do
                   atomically $ writeTMChan outChan (unlines (reverse results'))
                   pidM <- getPid (processHandle ghcmod')
                   print pidM
                   return DeadGhcMod
                 Alive results' -> do
                   atomically $ writeTMChan outChan (unlines (reverse results'))
                   return ghcmod'
          lift $ putStrLn $ "After consumption " <> show ghcmod''
          put ghcmod''
          nextAction ghcmod''


--------------------------------------------------------------------------------
data GhcMod
  = DeadGhcMod
  | ActiveGhcMod { stdoutHandle :: Handle
                 , stdinHandle :: Handle
                 , processHandle :: ProcessHandle}

--------------------------------------------------------------------------------
instance Show GhcMod where
  show DeadGhcMod = "non-running ghc-mod"
  show ActiveGhcMod{} = "running ghc-mod"


--------------------------------------------------------------------------------
port :: PortNumber
port = 4444


--------------------------------------------------------------------------------
getGhcProcess :: GhcMod -> IO GhcMod
getGhcProcess DeadGhcMod = mkGhcMod
getGhcProcess ghcmod@(ActiveGhcMod stdout stdin phandle) = do
  exitCodeM <- getProcessExitCode phandle
  case exitCodeM of
    Nothing -> return ghcmod
    Just _ -> do
      hClose stdout
      hClose stdin
      mkGhcMod


--------------------------------------------------------------------------------
-- Once a client connects, `talk` is spawned into a thread. `talk` does two
-- things:
-- * spawn a child of its own - ghcmodCommunicate which reads from an
-- inChannel, writes whatever messages it receives to ghcmod's stdin, reads the
-- response from ghcmod's stdout and writes that response to an outChannel
--
-- * run an infinite `issueCommands` loop which reads input from the client's
-- handle, checks to see if said input is (roughly) in a format we expect before
-- sending the input to the inChan for ghcmodCommunicate to process and then
-- waiting for the response on the outChan before printing it
talk :: Handle -> StateT GhcMod IO ()
talk h = do
  ghcmod <- get
  lift $ hSetBuffering h LineBuffering
  inChan <- lift newTMChanIO
  outChan <- lift newTMChanIO
  _ <- lift $
    forkFinally
      (runStateT (ghcmodCommunicate inChan outChan) ghcmod)
      (handleCommsDeath inChan outChan)
  _ <- lift $ runExceptT $ forever $ issueCommands inChan outChan
  return ()
  where
    handleCommsDeath inChan' outChan' exc = do
      putStrLn $ "Comms death because: " <> show exc
      putStrLn "communicate thread died"
      atomically (closeTMChan inChan')
      atomically (closeTMChan outChan')
      return ()
    issueCommands inChan' outChan' = do
      eof <- lift $ hIsEOF h
      when eof $ lift (handleCommsDeath inChan' outChan' ("Handle closed"::String)) >> throwE ()
      line <- lift $ hGetLine h
      if "check" `isPrefixOf` line || "lint" `isPrefixOf` line
        then do
          lift $ atomically $ writeTMChan inChan' line
          res' <- lift $ atomically $ readTMChan outChan'
          lift $
            putStrLn $ maybe "outChan is closed" ("GHC-MOD SEZ: " <>) res'
          when (isNothing res') $ trace "issueCommands loop over" $ throwE ()
        else lift $ putStrLn "Unknown command"


--------------------------------------------------------------------------------
doTalk :: Socket -> StateT GhcMod IO ()
doTalk sock = do
  ghcmod <- get
  forever $ do
        (conn,_) <- lift $ accept sock
        handle <- lift $ socketToHandle conn ReadWriteMode
        lift $ forkFinally
          (runStateT (talk handle) ghcmod)
          (\exc -> print exc >> hClose handle)


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
main :: IO ()
main =
  withSocketsDo $
  do sock <- socket AF_INET Stream 0
     setSocketOption sock ReuseAddr 1
     bind sock (SockAddrInet port iNADDR_ANY)
     listen sock 2
     putStrLn $ "listening on port " <> show port
     ghcmod <- mkGhcMod
     putStrLn "Made new process"
     runStateT (doTalk sock) ghcmod
     return ()
