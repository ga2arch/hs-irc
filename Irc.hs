

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
    write "NICK" nick
    write "USER" $ nick ++ " 0 * :tut bot"
    write "JOIN" channel
    s <- asks socket
    listen s
    return ()

write :: String -> String -> EventNet ()
write s t = do
      h <- asks socket
      liftIO $ hPrintf h "%s %s \r\n" s t
      --liftIO $ printf "> %s %s \n" s t

listen :: Handle -> EventNet ()
listen h = forever $ do
      t <- liftIO $ hGetLine h
      let s = init t
      if ping s then pong s else eval $ runP serverParser s
      liftIO $ putStrLn s
   where
      --clean = drop 1 . dropWhile (/= ':') . drop 1
      ping x = "PING :" `isPrefixOf` x
      pong x = write "PONG" $ ':' : drop 6 x

eval :: Maybe Message -> EventNet ()
eval (Just m) = dispatch $ runP userCmdParser $ message m
eval _ = return ()

dispatch :: Maybe UserCmd -> EventNet ()
dispatch (Just u) = case (cmd u) of
                  "id" -> privmsg $ args u
                  "sayHi" -> privmsg "Hi chico"
                  "broad" -> broadcast "test"
                  _ -> privmsg "not implemented"
dispatch _ = return ()

privmsg :: String -> EventNet ()
privmsg s = write "PRIVMSG" $ channel ++ " :" ++ s

msg :: Handle -> String -> IO ()
msg h s = hPrintf h "PRIVMSG %s \r\n" $ channel ++ " :" ++ s
