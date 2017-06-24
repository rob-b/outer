{-# LANGUAGE OverloadedStrings #-}
module Outer.Client where

import qualified Control.Exception          as X
import           Control.Monad              (unless)
import qualified Data.ByteString.Char8      as BC8
import qualified Data.ByteString.Lazy.Char8 as LBC8
import           Data.Monoid                ((<>))
import           Options.Applicative
import           System.IO                  (Handle, IOMode (ReadMode), hClose,
                                             hFileSize, openBinaryFile)
import           System.Socket
import           System.Socket.Family.Inet6
import           System.Socket.Protocol.TCP
import           System.Socket.Type.Stream


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
receive' sock = rr ""
  where
    rr acc = do
      res <- receive sock 4096 msgNoSignal
      if BC8.null res
        then pure acc
        else rr (acc <> res)


-------------------------------------------------------------------------------
-- | Load the given files using GHC and report errors/warnings
check :: Maybe BC8.ByteString -> BC8.ByteString -> IO ()
check mapFileM files = do
  let checkCmd = "check " <> files
  fileContent <- BC8.readFile (BC8.unpack files)
  sock <- setupSock 8080
  case mapFileM of
    Nothing -> do
      _ <-
        send' sock $
        BC8.intercalate
          ""
          [toNetString (checkCmd <> "\n") <> toNetString "\EOT"]
      response <- receive' sock
      BC8.putStrLn response
      close sock
    Just fileToMap -> do
      let msgs =
            BC8.intercalate
              ""
              [ toNetString $ "map-file " <> fileToMap
              , toNetString $ "\n" <> fileContent <> "\EOT\n"
              , toNetString "\EOT"
              ]
      _ <- send' sock msgs
      response <- receive' sock
      unless (response == "OK\n") $ BC8.putStrLn response
      close sock
      sock' <- setupSock 8080
      _ <-
        send' sock' $
        BC8.intercalate "" [toNetString checkCmd <> "\n" <> toNetString "\EOT"]
      response' <- receive' sock'
      close sock'
      unless (response' == "OK\n") $ BC8.putStrLn response'


--------------------------------------------------------------------------------
toNetString :: BC8.ByteString -> BC8.ByteString
toNetString s = BC8.intercalate "" [BC8.pack . show $ BC8.length s, ":", s]


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


-------------------------------------------------------------------------------
run :: Options -> IO ()
run (Options _ mapFile cmd) =
  case cmd of
    (Check files) -> check (BC8.pack <$> mapFile) (BC8.pack (unwords files))


-------------------------------------------------------------------------------
main :: IO ()
main = run =<< execParser argsWithInfo
