{-# LANGUAGE OverloadedStrings #-}
module Outer where

import qualified Control.Exception as X
import           Control.Monad     (forever, when)
import           Control.Retry     (RetryPolicyM, constantDelay, limitRetries,
                                    retrying)
import           Data.List         (isPrefixOf)
import           Data.Maybe
import           Data.Monoid       ((<>))
import           Data.Pool         (LocalPool, Pool, createPool,
                                    destroyResource, putResource, takeResource)
import           System.Exit       (ExitCode)
import           System.IO         (BufferMode (..), Handle, hGetLine,
                                    hPutStrLn, hSetBinaryMode, hSetBuffering,
                                    hSetEncoding, stdin, stdout, utf8)
import           System.IO.Error   (isEOFError)
import           System.Process    (ProcessHandle, StdStream (CreatePipe),
                                    createProcess, getProcessExitCode, proc,
                                    std_err, std_in, std_out, terminateProcess)


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
run :: IO ()
run = do
  hSetBuffering stdin LineBuffering
  hSetEncoding  stdin utf8

  hSetBuffering stdout LineBuffering
  hSetEncoding  stdout utf8
  pool <- createPool mkGhcMod killGhcMod 1 10 1
  forever $ poolio pool

  where
    def :: RetryPolicyM IO
    def = constantDelay 50000 <> limitRetries 2

    ghcCheck :: Pool GhcMod -> (GhcMod, LocalPool GhcMod) -> IO Bool
    ghcCheck pool (resource,local) = do
      exited' <- isJust <$> exited resource
      when exited' $ destroyResource pool local resource
      return exited'

    poolio :: Pool GhcMod -> IO ()
    poolio pool = do
      -- FIXME: add catch-all exception handler that destroys resource
      (resource,local) <-
        retrying def (\_ pair -> ghcCheck pool pair) (\_ -> takeResource pool)
      ret <- action resource
      putResource local resource
      return ret

    action :: GhcMod -> IO ()
    action ghc = do
      userInput <- getLine
      hPutStrLn (stdinHandle ghc) userInput
      result <- readUntilOk (stdoutHandle ghc) []
      putStrLn . unlines $ reverse result

    exited :: GhcMod -> IO (Maybe ExitCode)
    exited (ActiveGhcMod _ _ processHandle') = getProcessExitCode processHandle'
