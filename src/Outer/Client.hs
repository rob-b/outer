{-# LANGUAGE OverloadedStrings #-}
module Outer.Client where

import Debug.Trace
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy.Char8 as LBC8
import Data.Monoid ((<>))
import Options.Applicative
import System.IO (hFileSize, hClose, openBinaryFile, Handle, IOMode(ReadMode))
import System.Socket
import System.Socket.Family.Inet6
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream
import qualified Control.Exception             as X


data Options = Options
  { _optionsVerbose :: Maybe String
  , _optionsMapFile :: Maybe String
  , _optionsCommand :: Command
  } deriving (Show)


newtype Command =
  Check [String]
   deriving (Show)


-------------------------------------------------------------------------------
argsWithInfo :: ParserInfo Options
argsWithInfo =
  info (helper <*> optionParser) (fullDesc <> progDesc "Helper for ghc-mod interactive")


-------------------------------------------------------------------------------
optionParser :: Parser Options
optionParser =
  Options <$>
  optional (option str (short 'v' <> long "verbose" <> metavar "LEVEL")) <*>
  optional (strOption (long "map-file" <> metavar "MAPPING")) <*>
  commandParser


-------------------------------------------------------------------------------
commandParser :: Parser Command
commandParser =
  subparser $
  command
    "check"
    (withInfo
       checkParser
       "Load the given files using GHC and report errors/warnings, but don't produce output files")


-------------------------------------------------------------------------------
checkParser :: Parser Command
checkParser = Check <$> some (argument str (metavar "FILES.."))


-------------------------------------------------------------------------------
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


-------------------------------------------------------------------------------
setupSock :: Inet6Port -> IO (Socket Inet6 Stream TCP)
setupSock port = do
  sock <- socket :: IO (Socket Inet6 Stream TCP)
  connect sock (SocketAddressInet6 inet6Loopback port 0 0)
  return sock


-------------------------------------------------------------------------------
send' :: Socket f t p -> BC8.ByteString -> IO Int
send' sock msg = send sock msg msgNoSignal


-------------------------------------------------------------------------------
receive' :: Socket f t p -> IO BC8.ByteString
receive' sock = receive sock 10000 msgNoSignal


-------------------------------------------------------------------------------
-- | Load the given files using GHC and report errors/warnings
check :: Maybe BC8.ByteString -> BC8.ByteString -> IO ()
check mapFileM files = do
  fileContent <- tny
  let checkCmd = "check " <> files
  sock <- setupSock 8080
  case mapFileM of
    Nothing -> do
      _ <- send' sock checkCmd
      receive' sock >>= BC8.putStrLn
    Just fileToMap -> do
      let msgs =
            BC8.intercalate
              ""
              [ xx $ "map-file " <> fileToMap
              , xx $ "\n" <> fileContent <> "\EOT\n"
              , xx "\EOT"]
      _ <- send' sock msgs
      receive' sock >>= BC8.putStrLn

      close sock
      sock' <- setupSock 8080
      _ <- send' sock' $ BC8.intercalate "" [xx checkCmd, xx "\EOT"]
      receive' sock' >>= BC8.putStrLn


--------------------------------------------------------------------------------
xx :: BC8.ByteString -> BC8.ByteString
xx s = BC8.intercalate "" [BC8.pack . show $ BC8.length s, ":", s]


--------------------------------------------------------------------------------
fileOps :: IO (Integer, IO LBC8.ByteString)
fileOps =
  let open =
        X.try $ openBinaryFile "src/Outer.hs" ReadMode :: IO (Either X.IOException Handle)
      close' (Left _err) = return ()
      close' (Right h) = hClose h
  in X.bracket open close' $ \handle ->
      case handle of
        Left exc -> error $ "now what? " <> show exc
        Right handle' -> do
          size <- hFileSize handle'
          return (size, LBC8.hGetContents handle')


tny :: IO BC8.ByteString
tny = BC8.readFile "src/Outer.hs"


-------------------------------------------------------------------------------
run :: Options -> IO ()
run (Options _ mapFile cmd) =
  case cmd of
    (Check files) -> check (BC8.pack <$> mapFile) (BC8.pack (unwords files))


-------------------------------------------------------------------------------
main :: IO ()
main = run =<< execParser argsWithInfo
