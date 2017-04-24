{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Concurrent            (forkFinally, threadDelay)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TMChan
import qualified Control.Exception             as X
import           Control.Monad                 (forever, when)
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
                                                hSetBinaryMode, hSetBuffering)
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
        then return (line : acc)
        else readUntilOk handle (line : acc)


--------------------------------------------------------------------------------
go :: TMChan String -> TMChan String -> StateT GhcMod IO ()
go inChan outChan = forever go'
  where
    go' = do
      msgM <- lift $ atomically $ readTMChan inChan
      case msgM of
        Nothing -> error "Channel is closed. Do something!"
        Just msg -> do
          ghcmod <- get
          ghcmod' <- lift $ getGhcProcess ghcmod
          put ghcmod'
          lift $
            hPutStrLn (stdinHandle ghcmod') msg >>
            readUntilOk (stdoutHandle ghcmod') [] >>=
            \results ->
               atomically $ writeTMChan outChan (unlines (reverse results))


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
talk :: Handle -> StateT GhcMod IO ()
talk h = do
  ghcmod <- get
  lift $ hSetBuffering h LineBuffering
  inChan <- lift newTMChanIO
  outChan <- lift newTMChanIO
  lift $ forkFinally
    (runStateT (go inChan outChan) ghcmod)
    (\_ -> atomically (closeTMChan inChan) >> atomically (closeTMChan outChan))
  _ <- lift $ fmap (either id id) $ runExceptT $ forever $ loop inChan outChan
  return ()
  where
    loop inChan' outChan' =
      do
        line <- lift $ hGetLine h
        if "check" `isPrefixOf` line || "lint" `isPrefixOf` line
          then do
            lift $ atomically $ writeTMChan inChan' line
            res' <- lift $ atomically $ readTMChan outChan'
            lift $ putStrLn $ maybe "outChan is closed" (\r -> "result: " <> r) res'
            when (isNothing res') $ throwE ()
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
          (\_ -> hClose handle)


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
