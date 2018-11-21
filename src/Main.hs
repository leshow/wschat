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
import           Control.Concurrent                  ( MVar
                                                     , newMVar
                                                     , modifyMVar_
                                                     , modifyMVar
                                                     , readMVar
                                                     )
import           Control.Concurrent.Async            ( async )
import qualified Control.Concurrent.Chan.Unagi      as UC
import qualified Network.WebSockets                 as WS
import           Network.Wai.Handler.Warp            ( run )
import qualified Network.Wai.Handler.WebSockets     as WaiWS
import           ChatState

-- WS.ServerApp is an alias for PendingConnection -> IO ()
application :: UC.InChan Text -> MVar ServerState -> WS.ServerApp
application logChan state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    case T.split (== ' ') msg of
        ["/join", room, user] -> handle user conn room logChan state
        ["/list", room]       -> do
            s <- readMVar state
            WS.sendTextData
                conn
                ("Users: "
                    <> T.intercalate ", " (fromJust $ getUsernames room s) :: Text
                )
        _ -> WS.sendTextData conn ("Wrong announcement message." :: Text)


handle
    :: Text
    -> WS.Connection
    -> RoomName
    -> UC.InChan Text
    -> MVar ServerState
    -> IO ()
handle user conn room log clients = do
    state <- readMVar clients
    if
        | clientExists client room state -> WS.sendTextData
            conn
            ("User already exists" :: Text)
        | isFormatted -> WS.sendTextData
            conn
            ("Name cannot contain punctuation or whitespace and can't be empty" :: Text
            )
        | otherwise -> flip finally disconnect $ do
            modifyMVar_ clients $ \s -> pure $ joinRoom client room s
            s <- readMVar clients
            WS.sendTextData conn $ "Welcome! users: " <> T.intercalate
                ", "
                (fromJust $ getUsernames room s)
            broadcast log (fst client <> " joined " <> room) room s
            talk log client room clients
  where
    client = (user, conn)
    isFormatted =
        any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace]
    disconnect = do
        s <- modifyMVar clients $ \s -> do
            let s' = removeClient client room s
            pure (s', s')
        broadcast log (fst client <> " disconnected") room s

talk :: UC.InChan Text -> Client -> RoomName -> MVar ServerState -> IO ()
talk logChan (user, conn) room state = forever $ do
    msg <- WS.receiveData conn
    case T.split (== ' ') msg of
        ["/join", newroom] -> quit >> handle user conn newroom logChan state
        ["/leave"]         -> quit
        _                  -> do
            s <- readMVar state
            broadcast logChan (user <> ": " <> msg) room s
  where
    quit = do
        s <- modifyMVar state $ \s -> do
            let s' = removeClient (user, conn) room s
            pure (s', s')
        broadcast logChan (user <> " left room " <> room) room s

broadcast :: UC.InChan Text -> Text -> RoomName -> ServerState -> IO ()
broadcast logChan msg room state = do
    -- T.putStrLn msg
    let clients = getClients room state
    -- UC.writeChan logChan msg
    T.putStrLn msg
    case clients of
        Just members ->
            forM_ members $ \(_name, conn) -> WS.sendTextData conn msg
        Nothing -> pure ()

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
