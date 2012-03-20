{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Irc where

import Network
import System.IO
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import Data.List

import Parser

data Bot = Bot { socket :: Handle }
data Event = Connected
           | Disconnected
           | IrcCmd String
           | UserCmd String
           | Ping
  deriving (Show, Ord, Eq)

data Args = M Message
          | S String
          | D Message Cmd
          | None
  deriving (Show)

type Plugin = Args -> Event -> Int -> EIrc ()
type Plugins = [Plugin]
type Channel = String

type Irc = ReaderT Bot IO

newtype EIrc a = EI { unEI :: StateT MState Irc a}
        deriving (Monad, MonadState MState, MonadReader Bot, MonadIO)

type MState = Map.Map Event Plugins

runEIrc :: Bot -> EIrc a -> IO (a, MState)
runEIrc st action = runReaderT (runStateT (unEI action) Map.empty) st

subscribe :: (Event, Plugin) -> EIrc ()
subscribe (evt, fun) = do
    m <- get
    let funs = fromMaybe [] $ Map.lookup evt m
    put $ Map.insert evt (fun:funs) m
    return ()

unsubscribe :: (Event, Int) -> EIrc ()
unsubscribe (evt, i) = do
    m <- get
    let funs = fromMaybe [] $ Map.lookup evt m
    put $ Map.insert evt (delNth i funs) m
    return ()
  where
    delNth _ [] = []
    delNth n xs = take (n-1) xs ++ drop n xs

broadcast :: Event -> Args -> EIrc ()
broadcast evt args = do
    m <- get
    let funs = fromMaybe [] $ Map.lookup evt m
    mapM_ (\(f, i) -> f args evt i) $ zip funs (iterate (+1) 1)
