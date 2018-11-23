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
import qualified Network.Wai                        as Wai
import qualified Network.HTTP.Types                 as Http
import           ChatState

-- WS.ServerApp is an alias for PendingConnection -> IO ()
application :: TVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    num <- getInt
    case T.split (== ' ') msg of
        ["/join", room, user] -> handle (userN num user conn) room state
        ["/join"]             -> handle (userI num conn) "Main" state
        ["/list", room]       -> do
            s <- readTVarIO state
            WS.sendTextData
                conn
                ("Users: "
                    <> T.intercalate ", " (fromJust $ getUsernames room s) :: Text
                )
        _ -> WS.sendTextData conn ("Wrong announcement message." :: Text)


handle :: Client -> RoomName -> TVar ServerState -> IO ()
handle client@(_, name, conn) room clients = do
    state <- readTVarIO clients
    if
        | clientExists client room state -> WS.sendTextData
            conn
            ("User already exists" :: Text)
        | isFormatted name -> WS.sendTextData
            conn
            ("Name cannot contain punctuation or whitespace and can't be empty" :: Text
            )
        | otherwise -> flip finally leave $ do
            s <- atomically $ do
                modifyTVar' clients $ \s -> joinRoom client room s
                readTVar clients
            WS.sendTextData conn $ "Welcome! users: " <> T.intercalate
                ", "
                (fromJust $ getUsernames room s)
            broadcast (userHandle client <> " joined " <> room) room s
            talk client room clients
  where
    isFormatted :: Maybe Text -> Bool
    isFormatted Nothing = False
    isFormatted (Just n) =
        any ($ n) [T.null, T.any isPunctuation, T.any isSpace]
    leave :: IO ()
    leave = disconnect client room " disconnected" clients

talk :: Client -> RoomName -> TVar ServerState -> IO ()
talk client@(n, _, conn) room state = forever $ do
    msg <- WS.receiveData conn
    case T.split (== ' ') msg of
        ["/join", newroom] -> quit >> handle client newroom state
        ["/name", newname] -> quit >> handle (userN n newname conn) room state
        ["/leave"]         -> quit
        _                  -> do
            s <- readTVarIO state
            broadcast (userHandle client <> ": " <> msg) room s
  where
    quit :: IO ()
    quit = disconnect client room (" left room " <> room) state

broadcast :: Text -> RoomName -> ServerState -> IO ()
broadcast msg room state = do
    let clients = getClients room state
    T.putStrLn msg
    case clients of
        Just members ->
            forM_ members $ \(_, _, conn) -> WS.sendTextData conn msg
        Nothing -> pure ()

disconnect :: Client -> RoomName -> Text -> TVar ServerState -> IO ()
disconnect client room msg state = do
    s <- atomically $ do
        modifyTVar' state $ \s -> removeClient client room s
        readTVar state
    broadcast (userHandle client <> msg) room s

main :: IO ()
main = do
    state <- atomically $ newTVar newServerState
    -- (tx, rx) <- UC.newChan -- chan for logging
    -- _             <- async $ forever $ do
    --     msg <- UC.readChan rx
    --     T.putStrLn $ "log: " <> msg
    run 3000 $ WaiWS.websocketsOr WS.defaultConnectionOptions
                                  (application state)
                                  httpApp

httpApp :: Wai.Application
httpApp _ respond =
    respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"
