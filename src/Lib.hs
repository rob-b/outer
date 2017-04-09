{-# LANGUAGE OverloadedStrings #-}
module Lib where
 
import           Control.Concurrent            (forkFinally, threadDelay)
import           Control.Concurrent.STM        (atomically)
import           Control.Concurrent.STM.TMChan
import qualified Control.Exception             as X
import           Control.Monad                 (forever, when)
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


getPid :: ProcessHandle -> IO (Maybe PHANDLE)
getPid ph = withProcessHandle ph unpack
  where
    unpack ph_ =
      case ph_ of
        OpenHandle x -> return $ Just x
        ClosedHandle _ -> return Nothing


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


sleep :: Int -> IO ()
sleep x = threadDelay $ x * 1000


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


go :: TMChan String -> TMChan String -> IO ()
go inChan outChan = do
  (hin,hout,_,ph) <- mkProcess
  putStrLn "Made new process"
  go' hin hout ph
  where
    go' hin' hout' ph' = do
      msgM <- atomically $ readTMChan inChan
      case msgM of
        Nothing -> error "Channel is closed. Do something!"
        Just msg -> do
          exitCodeM <- getProcessExitCode ph'
          putStrLn $ maybe "ghc-mod is still running" (\ec -> "exit code: " <> show ec) exitCodeM
          case exitCodeM of
            Just _ -> do
              hClose hin'
              hClose hout'
              return ()
            Nothing -> do
              hPutStrLn hin' msg >> readUntilOk hout' [] >>=
                \results -> atomically $ writeTMChan outChan (unlines (reverse results))
              go' hin' hout' ph'


data GhcMod = GhcMod
  { stdoutHandle :: Handle
  , stdinHandle :: Handle
  , processHandle :: ProcessHandle
  }


main :: IO ()
main =
  withSocketsDo $
  do sock <- socket AF_INET Stream 0
     setSocketOption sock ReuseAddr 1
     bind sock (SockAddrInet port iNADDR_ANY)
     listen sock 2
     putStrLn $ "listening on port " <> show port
     forever $
       do (conn,_) <- accept sock
          handle <- socketToHandle conn ReadWriteMode
          forkFinally
            (talk handle)
            (\eE ->
                putStrLn (either show (const "no errors from talk") eE) >>
                putStrLn "The talk process died" >>
                hClose handle)


port :: PortNumber
port = 4444


talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  inChan <- newTMChanIO
  outChan <- newTMChanIO
  forkFinally
    (go inChan outChan)
    (\_ -> atomically (closeTMChan inChan) >> atomically (closeTMChan outChan))
  _ <- fmap (either id id) $ runExceptT $ forever $ loop inChan outChan
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
