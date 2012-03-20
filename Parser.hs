module Parser
       ( Message(..)
       , Cmd(..)
       , serverParser
       , userCmdParser
       , runP
       ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Monad

data Message = Message
     { nick :: String
     , command :: String
     , channel :: String
     , userMsg :: String
     }
  deriving (Show)

data Cmd = Cmd
     { cmd :: String
     , args :: String
     }
  deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

piece = many1 $ symbol <|> alphaNum

serverParser :: Parser Message
serverParser = Message
    <$> parseUsername
    <*> parseCmd
    <*> parseChan
    <*> (try (parseMessage <|> string ""))

parseUsername :: Parser String
parseUsername = between (char ':') (char '!') (many1 alphaNum)

parseCmd :: Parser String
parseCmd = piece >> between space space (many1 alphaNum)

parseChan :: Parser String
parseChan = try $ liftM ('#':) (between (char '#') space piece) <|> string ""

parseMessage :: Parser String
parseMessage = try $ char ':' >> many1 anyToken <|> string ""

userCmdParser :: Parser Cmd
userCmdParser = char '@' >>
              Cmd <$> piece
                  <*> liftM tail (try $ many1 anyToken <|> string " ")

runP :: Parser a -> String -> Maybe a
runP p input
        = case (parse p "" input) of
            Left err -> Nothing
            Right x  -> Just x
