{-# LANGUAGE OverloadedStrings #-}
module Lib where
import           Control.Concurrent       (forkFinally, forkIO, threadDelay)
import           Control.Concurrent.Chan
import qualified Control.Exception        as X
import           Control.Monad            (forever)
import           Data.List                (isPrefixOf)
import           Data.Monoid              ((<>))
import           Network                  (PortID (..), accept, listenOn)
import           Network.Socket           hiding (accept)
import           System.IO                (BufferMode (LineBuffering), Handle,
                                           hClose, hGetLine, hPutStrLn,
                                           hSetBinaryMode, hSetBuffering)
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
mkProcess =
  X.bracketOnError
    (createProcess
       (proc "ghc-mod" ["-b", "\n", "legacy-interactive"])
       { std_in = CreatePipe
       , std_out = CreatePipe
       , std_err = CreatePipe
       })
    shutDown
    work
  where
    work (Just hin,Just hout,Just herr,ph) = do
      hSetBuffering hin LineBuffering
      hSetBuffering herr LineBuffering
      hSetBuffering hout LineBuffering
      hSetBinaryMode hout False
      return (hin, hout, herr, ph)
    shutDown (Just hin,Just hout,_,ph) =
      getPid ph >>=
      \pid -> do
         putStrLn "what is the error"
         print pid
         terminateProcess ph >> waitForProcess ph
         hClose hin
         hClose hout


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


go :: Chan String -> Chan String -> IO ()
go inChan outChan = do
  (hin,hout,_,ph) <- mkProcess
  go' hin hout ph
  where
    go' hin' hout' ph' = do
      msg <- readChan inChan
      exitCodeM <- getProcessExitCode ph'
      case exitCodeM of
        Just _ -> do
          hClose hin'
          hClose hout'
        Nothing -> do
          hPutStrLn hin' msg >> readUntilOk hout' [] >>=
            \results -> writeChan outChan (unlines (reverse results))
          go' hin' hout' ph'


main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn (PortNumber(fromIntegral port))
  putStrLn $ "listening on port " <> show port
  forever $ do
    (handle, _, _) <- accept sock
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
      if "check" `isPrefixOf` line || "lint" `isPrefixOf` line
        then do
          writeChan inChan' line
          res' <- readChan outChan'
          putStrLn res'
          loop inChan' outChan'
        else putStrLn "2"
