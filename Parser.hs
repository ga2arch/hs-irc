module Parser
       ( Message(..)
       , UserCmd(..)
       , serverParser
       , userCmdParser
       , runP
       ) where

import Text.ParserCombinators.Parsec

data Message = Message { username :: String
                       , chan :: String
                       , command :: String
                       , message :: String
                       }
    deriving (Show)

data UserCmd = UserCmd { cmd :: String
                       , args :: String
                       }

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~."

word :: Parser String
word = many1 alphaNum

pstr :: Parser String
pstr = many1 $ symbol <|> alphaNum

parseServerLine :: Parser Message
parseServerLine = do
                char ':'
                server <- pstr
                space
                num <- word
                space
                username <- pstr
                space
                char ':'
                msg <- many1 $ symbol <|> alphaNum <|> space
                return Message { username = server
                               , chan = ""
                               , command = ""
                               , message = msg
                               }

parseLine :: Parser Message
parseLine = do
          nick <- parseUsername
          cmd <- parseCmd
          cn <- parseChan
          msg <- (parseMessage <|> string "")
          return msg
          return Message { username = nick
                         , chan = cn
                         , command = cmd
                         , message = msg
                         }

parseUsername :: Parser String
parseUsername = do
              char ':'
              username <- word
              char '!'
              return username

parseCmd :: Parser String
parseCmd = do
         _ <- pstr
         space
         cmd <- word
         space
         return cmd

parseChan :: Parser String
parseChan = do
          char '#'
          chan <- pstr
          space
          return $ '#':chan

parseMessage :: Parser String
parseMessage = do
             char ':'
             msg <- many1 $ symbol <|> alphaNum <|> space
             return msg

serverParser :: Parser Message
serverParser = parseLine


userCmdParser :: Parser UserCmd
userCmdParser = do
             char '@'
             cmd <- pstr
             args <- try (many1 $ symbol <|> alphaNum <|> space) <|> string ""
             return $ UserCmd cmd args

runP :: Parser a -> String -> Maybe a
runP p input
        = case (parse p "" input) of
            Left err -> Nothing
            Right x  -> Just x
