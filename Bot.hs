import Network
import System.IO
import System.IO.Error
import Text.Printf
import Data.List
import Control.Monad.Reader

import Irc
import Parser

main :: IO ()
main = do
    bot <- connect "irc.freenode.org" 6667
    runEIrc bot (loop [(Connected, onConnect),
                       (Connected, onTest),
                       (Ping, onPing),
                       (IrcCmd "PRIVMSG", onPrivmsg),
                       (UserCmd "id", onCmdId)])
    return ()

connect :: String -> Int -> IO Bot
connect server port = do
    h <- connectTo server $ PortNumber $ fromIntegral port
    hSetBuffering h NoBuffering
    return $ Bot h

disconnect :: Bot -> IO ()
disconnect = hClose . socket

loop :: [(Event, Plugin)] -> EIrc ()
loop ps = do
    st <- asks socket
    startPlugins ps
    broadcast Connected None
    forever $ do
        t <- liftIO $ hGetLine st
        let s = init t
        liftIO $ putStrLn s
        if ping s
            then broadcast Ping $ S (drop 6 s)
            else route $ runP serverParser s
  where
    ping x = "PING :" `isPrefixOf` x
    startPlugins = mapM_ subscribe
    route (Just m) = broadcast (IrcCmd $ command m) $ M m
    route _ = return ()

write :: String -> String -> EIrc ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s \r\n" s t

privmsg :: Channel -> String -> EIrc ()
privmsg chan s = write "PRIVMSG" $ chan ++ " :" ++ s

onConnect _ = do
    write "NICK" "lb-2"
    write "USER" $ "lb 0 * :tut bot"
    write "JOIN" "#bot-test"

onPing (S s) = do
    write "PONG :" s

onPrivmsg (M m) = if "@" `isPrefixOf` (userMsg m)
                     then route $ runP userCmdParser (userMsg m)
                     else return ()
  where
    route (Just c) = broadcast (UserCmd $ cmd c) $ D m c
    route _ = return ()

onCmdId (D m c) = do
    privmsg (channel m) $ "Hai detto:" ++ (args c)

onTest _ = do
    liftIO $ print "Connesso"
    return ()
