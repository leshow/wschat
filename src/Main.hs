module Main where

import           Data.Char                           ( isPunctuation
                                                     , isSpace
                                                     )
import           Data.Maybe                          ( fromJust )
import           Data.Text                           ( Text )
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import           Control.Exception                   ( finally )
import           Control.Monad                       ( forM_
                                                     , forever
                                                     )
import           Control.Concurrent.STM              ( TVar
                                                     , newTVar
                                                     , modifyTVar'
                                                     , readTVar
                                                     , readTVarIO
                                                     , atomically
                                                     )
import           Control.Concurrent.Async            ( async )
import qualified Control.Concurrent.Chan.Unagi      as UC
import qualified Network.WebSockets                 as WS
import           Network.Wai.Handler.Warp            ( run )
import qualified Network.Wai.Handler.WebSockets     as WaiWS
import           ChatState

-- WS.ServerApp is an alias for PendingConnection -> IO ()
application :: TVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    case T.split (== ' ') msg of
        ["/join", room, user] -> handle user conn room state
        ["/list", room]       -> do
            s <- readTVarIO state
            WS.sendTextData
                conn
                ("Users: "
                    <> T.intercalate ", " (fromJust $ getUsernames room s) :: Text
                )
        _ -> WS.sendTextData conn ("Wrong announcement message." :: Text)


handle :: Text -> WS.Connection -> RoomName -> TVar ServerState -> IO ()
handle user conn room clients = do
    state <- readTVarIO clients
    if
        | clientExists client room state -> WS.sendTextData
            conn
            ("User already exists" :: Text)
        | isFormatted -> WS.sendTextData
            conn
            ("Name cannot contain punctuation or whitespace and can't be empty" :: Text
            )
        | otherwise -> flip finally disconnect $ do
            s <- atomically $ do
                modifyTVar' clients $ \s -> joinRoom client room s
                readTVar clients
            WS.sendTextData conn $ "Welcome! users: " <> T.intercalate
                ", "
                (fromJust $ getUsernames room s)
            broadcast (fst client <> " joined " <> room) room s
            talk client room clients
  where
    client = (user, conn)
    isFormatted =
        any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace]
    disconnect = do
        s <- atomically $ do
            modifyTVar' clients $ \s -> removeClient client room s
            readTVar clients
        broadcast (fst client <> " disconnected") room s

talk :: Client -> RoomName -> TVar ServerState -> IO ()
talk (user, conn) room state = forever $ do
    msg <- WS.receiveData conn
    case T.split (== ' ') msg of
        ["/join", newroom] -> quit >> handle user conn newroom state
        ["/leave"]         -> quit
        _                  -> do
            s <- readTVarIO state
            broadcast (user <> ": " <> msg) room s
  where
    quit = do
        s <- atomically $ do
            modifyTVar' state $ \s -> removeClient (user, conn) room s
            readTVar state
        broadcast (user <> " left room " <> room) room s

broadcast :: Text -> RoomName -> ServerState -> IO ()
broadcast msg room state = do
    let clients = getClients room state
    T.putStrLn msg
    case clients of
        Just members ->
            forM_ members $ \(_name, conn) -> WS.sendTextData conn msg
        Nothing -> pure ()

main :: IO ()
main = do
    state <- atomically $ newTVar newServerState
    -- (tx, rx) <- UC.newChan -- chan for logging
    -- _             <- async $ forever $ do
    --     msg <- UC.readChan rx
    --     T.putStrLn $ "log: " <> msg
    run
        3000
        (WaiWS.websocketsOr WS.defaultConnectionOptions
                            (application state)
                            undefined
        )
