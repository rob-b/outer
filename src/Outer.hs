{-# LANGUAGE OverloadedStrings #-}
module Outer where

-- import Control.Retry
import System.Process
       (ProcessHandle, std_in, std_out, std_err, createProcess, proc,
        StdStream(CreatePipe))
import System.IO
       (Handle, BufferMode(LineBuffering), hSetBuffering, hSetBinaryMode)
import           Data.Monoid                   ((<>))
import Network.Socket


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
data GhcMod = ActiveGhcMod
  { stdoutHandle :: Handle
  , stdinHandle :: Handle
  , processHandle :: ProcessHandle
  }


--------------------------------------------------------------------------------
port :: PortNumber
port = 4444


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
     -- runStateT (_) ghcmod
     return ()
