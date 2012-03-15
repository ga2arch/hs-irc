import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent
import Prelude hiding (catch)
import Parser


server = "irc.freenode.org"
port = 6667
channel = "#bot-test"
nick = "tbotter"

data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

main :: IO ()
main = do
     st <- connect
     loop st
     return ()
     {-forever $ do
             putStr ">> " >> hFlush stdout
             l <- getLine
             msg (socket st) l
     -}
  where
    disconnect = hClose . socket
    loop st =  runReaderT run st

connect :: IO Bot
connect = do
        h <- connectTo server $ PortNumber $ fromIntegral port
        hSetBuffering h NoBuffering
        return $ Bot h

run :: Net ()
run = do
    write "NICK" nick
    write "USER" $ nick ++ " 0 * :tut bot"
    write "JOIN" channel
    s <- asks socket
    listen s
    return ()

write :: String -> String -> Net ()
write s t = do
      h <- asks socket
      liftIO $ hPrintf h "%s %s \r\n" s t
      --liftIO $ printf "> %s %s \n" s t

listen :: Handle -> Net ()
listen h = forever $ do
      t <- liftIO $ hGetLine h
      let s = init t
      if ping s then pong s else eval $ runP serverParser s
      liftIO $ putStrLn s
   where
      --clean = drop 1 . dropWhile (/= ':') . drop 1
      ping x = "PING :" `isPrefixOf` x
      pong x = write "PONG" $ ':' : drop 6 x

eval :: Maybe Message -> Net ()
eval (Just m) = dispatch $ runP userCmdParser $ message m
eval _ = return ()

dispatch :: Maybe UserCmd -> Net ()
dispatch (Just u) = case (cmd u) of
                  "id" -> privmsg $ args u
                  "sayHi" -> privmsg "Hi chico"
                  _ -> privmsg "not implemented"
dispatch _ = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" $ channel ++ " :" ++ s

msg :: Handle -> String -> IO ()
msg h s = hPrintf h "PRIVMSG %s \r\n" $ channel ++ " :" ++ s
