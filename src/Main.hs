module Main where

import           Data.Char                      ( isPunctuation
                                                , isSpace
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Control.Exception              ( finally )
import           Control.Monad                  ( forM_
                                                , forever
                                                )
import           Control.Concurrent             ( MVar
                                                , newMVar
                                                , modifyMVar_
                                                , modifyMVar
                                                , readMVar
                                                )
import           Control.Concurrent.Async       ( async )
import qualified Control.Concurrent.Chan.Unagi as UC
import qualified Network.WebSockets            as WS
import           Network.Wai.Handler.Warp       ( run )
import qualified Network.Wai.Handler.WebSockets
                                               as WaiWS

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient = (:)

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: UC.InChan Text -> Text -> ServerState -> IO ()
broadcast logChan msg clients = do
    -- T.putStrLn msg
    UC.writeChan logChan msg
    forM_ clients $ \(_name, conn) -> WS.sendTextData conn msg

-- WS.ServerApp is an alias for PendingConnection -> IO ()
application :: UC.InChan Text -> MVar ServerState -> WS.ServerApp
application logChan state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg     <- WS.receiveData conn
    clients <- readMVar state
    let client = (T.drop (T.length prefix) msg, conn)
    case msg of
        _
            | not (T.isPrefixOf prefix msg) -> WS.sendTextData
                conn
                ("Wrong announcment message." :: Text)
            | isFormatted client -> WS.sendTextData
                conn
                ("Name cannot contain punctuation or whitespace and can't be empty" :: Text
                )
            | clientExists client clients -> WS.sendTextData
                conn
                ("User already exists" :: Text)
            | otherwise -> flip finally (disconnect client) $ do
                modifyMVar_ state $ \s -> do
                    let s' = addClient client s
                    WS.sendTextData conn $ "Welcome! users: " <> T.intercalate
                        ", "
                        (fmap fst s)
                    broadcast logChan (fst client <> " joined") s'
                    pure s'
                talk logChan client state
  where
    prefix = "connect "
    isFormatted client =
        any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace]
    disconnect client = do
        s <- modifyMVar state $ \s -> do
            let s' = removeClient client s
            pure (s', s')
        broadcast logChan (fst client <> " disconnected") s

talk :: UC.InChan Text -> Client -> MVar ServerState -> IO ()
talk logChan (user, conn) state = forever $ do
    msg <- WS.receiveData conn
    s   <- readMVar state
    broadcast logChan (user <> ": " <> msg) s

main :: IO ()
main = do
    state         <- newMVar newServerState
    (logChan, rx) <- UC.newChan -- chan for logging
    _             <- async $ forever $ do
        msg <- UC.readChan rx
        T.putStrLn $ "log: " <> msg
    run
        3000
        (WaiWS.websocketsOr WS.defaultConnectionOptions
                            (application logChan state)
                            undefined
        )
