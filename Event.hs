{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Event where

import System.IO
import Network
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State
import qualified Data.Map as Map
import Prelude hiding (lookup)

data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

newtype EventNet a = EN {unEN :: StateT MyState Net a}
      deriving (Monad,MonadState MyState,MonadReader Bot,MonadIO)

type MyState = Map.Map String [(String -> EventNet ())]

runEventNet :: Bot -> EventNet a -> IO (a, MyState)
runEventNet st action = runReaderT (runStateT (unEN action) Map.empty) st

subscribe :: String -> (String -> EventNet ()) -> EventNet ()
subscribe evt fun = do
                  m <- get
                  let funs = lookup evt m
                  let nm = Map.insert evt (fun:funs) m
                  put nm
                  return ()

broadcast :: String -> EventNet ()
broadcast evt args = do
          m <- get
          let funs = lookup evt m
          mapM (\f -> f args) funs
          return ()

lookup :: String
       -> Map.Map String [(String -> EventNet ())]
       -> [(String -> EventNet ())]
lookup evt m = case (Map.lookup evt m) of
       Nothing -> [(\x -> return ())]
       Just funs  -> funs
