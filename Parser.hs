module Parser
       ( Message(..)
       , UserCmd(..)
       , serverParser
       , userCmdParser
       , runP
       ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Control.Monad

data Message = Message { username :: String
                       , command :: String
                       , chan :: String
                       , message :: String
                       }
    deriving (Show)

data UserCmd = UserCmd { cmd :: String
                       , args :: String
                       }
    deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

word :: Parser String
word = many1 alphaNum

pstr :: Parser String
pstr = many1 $ symbol <|> alphaNum

parseLine :: Parser Message
parseLine = Message <$> parseUsername
          <*> parseCmd
          <*> parseChan
          <*> (parseMessage <|> string "")

parseUsername :: Parser String
parseUsername = between (char ':') (char '!') word

parseCmd :: Parser String
parseCmd = pstr >> between space space word

parseChan :: Parser String
parseChan = liftM ('#':) (between (char '#') space pstr)

parseMessage :: Parser String
parseMessage = (char ':') >> (many1 $ symbol <|> alphaNum <|> space)

serverParser :: Parser Message
serverParser = parseLine

userCmdParser :: Parser UserCmd
userCmdParser = (char '@') >> (UserCmd <$> pstr
              <*> (try (many1 $ symbol <|> alphaNum <|> space) <|> string ""))

runP :: Parser a -> String -> Maybe a
runP p input
        = case (parse p "" input) of
            Left err -> Nothing
            Right x  -> Just x
