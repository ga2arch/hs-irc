

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

test :: String -> EventNet ()
test x = privmsg "Working !!"

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
    s <- asks socket
    ircLoop s
    return ()

write :: String -> String -> EventNet ()
write s t = do
      h <- asks socket
      liftIO $ hPrintf h "%s %s \r\n" s t
      --liftIO $ printf "> %s %s \n" s t

ircLoop :: Handle -> EventNet ()
ircLoop h = forever $ do
        t <- liftIO $ hGetLine h
        let s = init t
        if ping s then broadcast "ping" s else eval $ runP serverParser s
   where
        ping x = "PING :" `isPrefixOf` x

eval :: Maybe Message -> EventNet ()
eval (Just m) = if "@" `isPrefixOf` msg
                then dispatch $ runP userCmdParser msg
                else return ()
     where
        msg = message m
        dispatch (Just u) = broadcast (cmd u) (args u)
        dispatch _ = return ()
eval _ = return ()

privmsg :: String -> EventNet ()
privmsg s = write "PRIVMSG" $ channel ++ " :" ++ s

msg :: Handle -> String -> IO ()
msg h s = hPrintf h "PRIVMSG %s \r\n" $ channel ++ " :" ++ s



pong :: String -> EventNet ()
pong x = write "PONG" $ ':' : drop 6 x

cmdId :: String -> EventNet ()
cmdId x = privmsg x

cmdHpaste :: String -> EventNet ()
cmdHpaste x = privmsg "http://hpaste.org"
