module Outer.Comm where

import qualified Control.Exception           as X
import           Control.Monad               (when)
import           Control.Monad.Trans.Control (control)
import           Control.Retry               (RetryPolicyM, constantDelay,
                                              limitRetries, retrying)
import           Data.Maybe                  (isJust)
import           Data.Monoid                 ((<>))
import           Data.Pool                   (LocalPool, Pool, createPool,
                                              destroyResource, putResource,
                                              takeResource)
import           System.Exit                 (ExitCode)
import           System.IO                   (BufferMode (..), Handle,
                                              hSetBinaryMode, hSetBuffering)
import           System.Process              (ProcessHandle,
                                              StdStream (CreatePipe),
                                              createProcess, getProcessExitCode,
                                              proc, std_err, std_in, std_out,
                                              terminateProcess)


--------------------------------------------------------------------------------
data GhcMod = ActiveGhcMod
  { stdoutHandle :: Handle
  , stdinHandle :: Handle
  , processHandle :: ProcessHandle
  }


--------------------------------------------------------------------------------
-- | Check if a GhcMod process retrieved from the pool has died. If it has,
-- remove that instance from the pool
ghcCheck :: Pool GhcMod -> (GhcMod, LocalPool GhcMod) -> IO Bool
ghcCheck pool (resource,local) = do
  exited' <- isJust <$> exited resource
  when exited' $ destroyResource pool local resource
  return exited'


--------------------------------------------------------------------------------
-- | Try at most two times to obtain a running GhcMod instance from the pool
retryghc :: Pool GhcMod -> IO (GhcMod, LocalPool GhcMod)
retryghc pool =
  let checker pool' _status = ghcCheck pool'
      policy :: RetryPolicyM IO
      policy = constantDelay 250000 <> limitRetries 2
  in retrying policy (checker pool) (const $ takeResource pool)


--------------------------------------------------------------------------------
poolio :: Pool GhcMod -> (GhcMod -> IO ()) -> IO ()
poolio pool fn =
  control $
  \runInIO ->
     X.mask $
     \restore -> do
       (resource,local) <- retryghc pool
       ret <-
         restore (runInIO (fn resource)) `X.onException`
         destroyResource pool local resource
       putResource local resource
       return ret


--------------------------------------------------------------------------------
exited :: GhcMod -> IO (Maybe ExitCode)
exited (ActiveGhcMod _ _ processHandle') = getProcessExitCode processHandle'


--------------------------------------------------------------------------------
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
createPool' :: IO (Pool GhcMod)
createPool' = createPool mkGhcMod killGhcMod 1 10 1
