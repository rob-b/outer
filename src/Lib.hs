{-# LANGUAGE OverloadedStrings #-}
module Lib where
import           Control.Concurrent       (forkFinally, forkIO,
                                           threadDelay)
import           Control.Concurrent.Chan
import qualified Control.Exception        as X
import           Control.Monad            (forever)
import           Data.List                (isPrefixOf)
import           Data.Monoid              ((<>))
import           Network                  (PortID(..), listenOn, accept)
import           Network.Socket           hiding (accept)
import           System.IO
import           System.IO.Error          (isEOFError)
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
        then return acc
        else readUntilOk handle (line : acc)


go :: Chan String -> Chan String -> IO b
go inChan outChan = do
  (hin,hout,herr,ph) <- mkProcess
  let go' = do
        msg <- readChan inChan
        exitCodeM <- getProcessExitCode ph
        case exitCodeM of
          Just _ -> error "process died; should restart it here"
          Nothing ->
            hPutStrLn hin msg >> readUntilOk hout [] >>=
            \results -> writeChan outChan (unlines (reverse results))
  forever go'


main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber(fromIntegral port))
  putStrLn $ "listening on port " <> show port
  forever $ do
    (handle, host, port) <- accept sock
    forkFinally (talk handle) (\_ -> hClose handle)


port :: Int
port = 4444


talk :: Handle -> IO ()
talk h = do
  hSetBuffering h LineBuffering
  inChan <- newChan
  outChan <- newChan
  forkIO $ go inChan outChan
  loop inChan outChan
  where
    loop inChan' outChan' = do
      line <- hGetLine h
      if "check" `isPrefixOf` line
        then do
          writeChan inChan' line
          res' <- readChan outChan'
          putStrLn res'
          loop inChan' outChan'
        else putStrLn "2"
