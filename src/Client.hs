module Client where

import           Control.Concurrent.Async       ( async )
import           Control.Monad                  ( forever
                                                , unless
                                                )
import           Control.Monad.Trans            ( liftIO )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Network.WebSockets            as WS

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    _ <- async $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop
    loop
    WS.sendClose conn ("Exiting..." :: Text)

main :: IO ()
main = WS.runClient "127.0.0.1" 3000 "/" app
