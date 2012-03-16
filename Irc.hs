

import System.IO
import Network
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State
import qualified Data.Map as Map
import Text.Printf
import Data.List
import System.Exit
import Control.Arrow
import Control.Exception
import Control.Concurrent
import Prelude hiding (catch)
import Parser
import Event

server = "irc.freenode.org"
port = 6667
channel = "#bot-test"
nick = "lambot"

main :: IO ()
main = do
     st <- connect
     runEventNet st run
     return ()
  where
    disconnect = hClose . socket

connect :: IO Bot
connect = do
        h <- connectTo server $ PortNumber $ fromIntegral port
        hSetBuffering h NoBuffering
        return $ Bot h

run :: EventNet ()
run = do
    mapM subscribe [("ping", pong),
                    ("id", cmdId),
                    ("hpaste", cmdHpaste)]
    write "NICK" nick
    write "USER" $ nick ++ " 0 * :tut bot"
    write "JOIN" channel
    write "JOIN" "#bot-test2"
    s <- asks socket
    ircLoop s
    return ()

write :: String -> String -> EventNet ()
write s t = do
      h <- asks socket
      liftIO $ hPrintf h "%s %s \r\n" s t
      liftIO $ printf "> %s %s \n" s t

ircLoop :: Handle -> EventNet ()
ircLoop h = forever $ do
        t <- liftIO $ hGetLine h
        let s = init t
        liftIO $ putStrLn s
        if ping s then broadcast "ping" "" s else eval $ runP serverParser s
   where
        ping x = "PING :" `isPrefixOf` x

eval :: Maybe Message -> EventNet ()
eval (Just m) = if "@" `isPrefixOf` (message m)
                then dispatch (chan m) $ runP userCmdParser (message m)
                else return ()
     where
        dispatch chan (Just u) = broadcast (cmd u) chan (args u)
        dispatch _ _ = return ()
eval _ = return ()

privmsg :: String -> String -> EventNet ()
privmsg chan s = do
        write "PRIVMSG" $ chan ++ " :" ++ s
        return ()

pong :: String -> String -> EventNet ()
pong _ x = write "PONG" $ ':' : drop 6 x

cmdId :: String -> String -> EventNet ()
cmdId chan x = privmsg chan x

cmdHpaste :: String -> String -> EventNet ()
cmdHpaste chan x = privmsg chan "http://hpaste.org"
