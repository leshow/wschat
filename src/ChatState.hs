module ChatState where

import           Data.Text                           ( Text )
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import           System.Random                       ( getStdRandom
                                                     , randomR
                                                     )
import qualified Data.Map.Strict                    as Map
import           Data.Map.Strict                     ( Map )
import qualified Network.WebSockets                 as WS
import           Data.List                           ( foldl' )

type RoomName = Text
-- Map of roomname [(username, conn)]
type Rooms = Map RoomName [Client]
-- (username, conn)
type Client = (Int, Maybe Text, WS.Connection)
-- Map of roomName [(username, msg)]
type History = Map RoomName [(Text, Text)]

data ServerState = ServerState
    { rooms :: !Rooms
    , history :: !History
    }

data Action
    = JoinRoom (RoomName, Client)
    | LeaveRoom (RoomName, Text)
    | ListRooms
    | SendMessage (RoomName, Text, Text)
    | GetHistory RoomName

newServerState :: ServerState
newServerState = ServerState { rooms = Map.empty, history = Map.empty }

numClients :: ServerState -> Int
numClients state = foldl' (\acc (_, clients) -> acc + length clients) 0
    $ Map.toList (rooms state)

clientExists :: Client -> RoomName -> ServerState -> Bool
clientExists client room state = case Map.lookup room (rooms state) of
    Just clients -> any ((== getNum client) . getNum) clients
    Nothing      -> False

joinRoom :: Client -> RoomName -> ServerState -> ServerState
joinRoom client room state = case Map.lookup room (rooms state) of
    Just clients ->
        state { rooms = Map.insert room (client : clients) (rooms state) }
    Nothing -> state { rooms = Map.insert room [client] (rooms state) }

removeClient :: Client -> RoomName -> ServerState -> ServerState
removeClient client room state = case Map.lookup room (rooms state) of
    Just clients -> state
        { rooms = Map.insert room
                             (filter ((/= getNum client) . getNum) clients)
                             (rooms state)
        }
    Nothing -> state

getClients :: RoomName -> ServerState -> Maybe [Client]
getClients room state = Map.lookup room (rooms state)

getUsernames :: RoomName -> ServerState -> Maybe [Text]
getUsernames room state = fmap userHandle <$> getClients room state

getInt :: IO Int
getInt = getStdRandom (randomR (0, maxBound :: Int))

getNum :: Client -> Int
getNum (!i, _, _) = i

getName :: Client -> Maybe Text
getName (_, !name, _) = name

userI :: Int -> WS.Connection -> Client
userI num conn = (num, Nothing, conn)

userN :: Int -> Text -> WS.Connection -> Client
userN num name conn = (num, Just name, conn)

userHandle :: Client -> Text
userHandle (_  , Just name, _) = name
userHandle (num, Nothing  , _) = T.pack $ show num
