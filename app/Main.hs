{-
:set -package optparse-applicative
:load app/Main.hs
-}
module Main where

import LocalPrelude

import Control.Exception
import Options.Applicative qualified as O
import Network.Socket as N
import NetworkSimpler
import System.IO

import Protocols
import Interpreters qualified
import Interpreters.Socket qualified as Socket
import DSL


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  cmd <- O.execParser $ mkCli $ command
  case cmd of
    Node_ (Node label left right client) -> do
      let log_ msg_ = putStrLn $ "[" <> label <> "]: " <> msg_

      log_ $ "left: " <> show left
      log_ $ "right: " <> show right
      log_ $ "client: " <> show client

      let
        getSockets = do
          leftSocket' <- createTCP' left
          rightSocket <- connectTcpLoop right (\_e -> putStrLn "Connecting.." >> sleep 1)
          leftSocket <- fst <$> N.accept leftSocket'
          log_ "Nodes connected!"
          clientSocket' <- createTCP' client
          return (leftSocket, rightSocket, clientSocket')

        closeSockets (_l, r, _c) = do
          log_ "Closing sockets.."
          N.gracefulClose r 5000
          log_ "Closing sockets done."

      bracket getSockets closeSockets $ \(leftSocket, rightSocket, clientSocket') -> do
        log_ $ "Waiting for client on " <> show client <> ".. "
        clientSocket <- fst <$> N.accept clientSocket'
        log_ $ "Client connected!"
        arg1 <- Socket.receiveWord clientSocket
        arg2 <- Socket.receiveWord clientSocket
        log_ $ "Shares received: " <> show (arg1, arg2)
        res <- Interpreters.run (Protocols.multiply arg1 arg2) (Socket.connectedSocketConf ioRandom (\msg' -> putStrLn $ "[" <> label <> "] " <> msg') leftSocket rightSocket)
        log_ $ "Result computed: " <> show res
        Socket.sendWord clientSocket res
        log_ $ "Result to cilent sent."

    Client_ (Client e1 e2 e3 arg1 arg2) -> do
      putStrLn "Creating shares"
      (a1n1, a1n2, a1n3) <- share arg1
      (a2n1, a2n2, a2n3) <- share arg2

      let
        connectNodes = do
          putStrLn "Connect nodes"
          n1 <- connectTcpLoop e1 (\_e -> putStrLn "Connecting.." >> sleep 1)
          n2 <- connectTcpLoop e2 (\_e -> putStrLn "Connecting.." >> sleep 1)
          n3 <- connectTcpLoop e3 (\_e -> putStrLn "Connecting.." >> sleep 1)
          return (n1, n2, n3)
        closeSockets (n1, n2, n3) = do
          N.gracefulClose n1 5000
          N.gracefulClose n2 5000
          N.gracefulClose n3 5000

      r <- bracket connectNodes closeSockets $ \(n1, n2, n3) -> do
        putStrLn "Send arguments"
        Socket.sendWord n1 a1n1
        Socket.sendWord n1 a2n1
        Socket.sendWord n2 a1n2
        Socket.sendWord n2 a2n2
        Socket.sendWord n3 a1n3
        Socket.sendWord n3 a2n3


        putStrLn "Receive results"
        n1r <- Socket.receiveWord n1
        n2r <- Socket.receiveWord n2
        n3r <- Socket.receiveWord n3
        return (n1r, n2r, n3r)

      print $ unshare r

data Command = Node_ Node | Client_ Client
data Node = Node String Endpoint Endpoint Endpoint
data Client = Client Endpoint Endpoint Endpoint Word Word

command :: O.Parser Command
command = O.subparser
   $ O.command "node" (Node_ <$> mkCli node)
  <> O.command "client" (Client_ <$> mkCli client)
  where
    node :: O.Parser Node
    node = Node
      <$> O.strOption (O.long "label" <> O.metavar "NODE-LABEL")
      <*> O.argument endpoint metaEndpoint
      <*> O.argument endpoint metaEndpoint
      <*> O.argument endpoint metaEndpoint

    client :: O.Parser Client
    client = Client
      <$> O.argument endpoint metaEndpoint
      <*> O.argument endpoint metaEndpoint
      <*> O.argument endpoint metaEndpoint
      <*> O.argument O.auto metaWord
      <*> O.argument O.auto metaWord

endpoint :: O.ReadM Endpoint
endpoint = O.eitherReader p
  where
    p :: String -> Either String Endpoint
    p str = let (hostStr, portStr_) = span (/= ':') str
      in case portStr_ of
           ':' : portStr -> Endpoint <$> host hostStr <*> readEitherLabel "port" portStr
           _ -> Left "No port"

    host :: String -> Either String HostAddress
    host = \case
      "localhost" -> Right localhostIpv4
      "" -> Right localhostIpv4
      str -> Left $ "No parse as host: " <> str

metaEndpoint :: O.Mod O.ArgumentFields a
metaEndpoint = O.metavar "ENDPOINT"

metaWord :: O.Mod O.ArgumentFields a
metaWord = O.metavar "WORD"

readEitherLabel :: Read b => String -> String -> Either String b
readEitherLabel label str = case readEither str of
  Right a -> Right a
  Left msg' -> Left $ msg' <> ", " <> label <> ": " <> str

mkCli :: O.Parser a -> O.ParserInfo a
mkCli parser = O.info (O.helper <*> parser) $ O.fullDesc <> O.header "mpc" <> O.progDesc ""
