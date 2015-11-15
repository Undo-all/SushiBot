{-# LANGUAGE OverloadedStrings #-}

module Bot 
( Command(..)
, Special(..)
, Bot(..)
, connectBot
, privmsg
, action
) where

import Network
import Data.Char
import Data.List
import System.IO
import Data.Maybe
import Control.Monad (when)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Command = Command 
               { commandName :: T.Text
               , commandDesc :: T.Text
               , commandNumArgs :: (Int, Maybe Int)
               , commandFunc :: [T.Text] -> T.Text -> Bot -> IO ()
               }

data Special = Special
               { specialCond :: T.Text -> Bool
               , specialFunc :: T.Text -> Bot -> IO ()
               }  

data Bot = Bot
           { botServer :: T.Text
           , botPort :: Int
           , botNick :: T.Text
           , botName :: T.Text
           , botChannel :: T.Text
           , botLogging :: Bool
           , botCommands :: M.Map T.Text Command
           , botSpecials :: [Special]
           , botHandle :: Handle
           }

privmsg :: Bot -> T.Text -> IO ()
privmsg b s = T.hPutStr (botHandle b) $ T.concat ["PRIVMSG ", botChannel b, " :", s, "\n"]

action :: Bot -> T.Text -> IO ()
action b s = T.hPutStr (botHandle b) $ T.concat ["PRIVMSG ", botChannel b, " :\0001ACTION ", s, "\0001\n"]

connectBot :: T.Text -> Int -> T.Text -> T.Text -> T.Text -> 
              Bool -> [Command] -> [Special] -> IO ()
connectBot server port nick name chan logging comms specs =
    do h <- connectTo (T.unpack server) (PortNumber (fromIntegral port))
       hSetBuffering h NoBuffering
    
       T.hPutStr h $ T.concat ["USER ", nick, " ", nick, " ", nick, " :", name, "\n"]
       T.hPutStr h $ T.concat ["NICK ", nick, "\n"]

       let commsMap = M.fromList $ map (\c@(Command n _ _ _) -> (n, c)) comms
           b        = Bot server port nick name chan logging commsMap specs h 

       botLoop h b

botLoop :: Handle -> Bot -> IO ()
botLoop h b = do s <- T.hGetLine h
                 when (botLogging b) $ print s 
                 handleData s h b
                 botLoop h b

handleData :: T.Text -> Handle -> Bot -> IO ()
handleData s h b
    | isPing         = T.hPutStr (botHandle b) (T.concat ["PONG ", T.drop 4 s, "\n"])
    | isMode         = T.hPutStr (botHandle b) (T.concat ["JOIN ", botChannel b, "\n"])
    | isEmpty        = return ()
    | isJust special = maybe (error "nope.") (\x -> specialFunc x (clean s) b) special
    | isCommand s    = eval ((\(x:xs) -> T.tail x : xs) $ space $ T.words (clean s)) (username s) b
    | otherwise      = return ()
    where isCommand m = "PRIVMSG" `T.isInfixOf` m &&
                        T.head (clean m) == '!'
          clean       = T.tail . T.dropWhile (/= ':') . T.tail
          space       = map (T.map (\x -> if x == '_' then ' ' else x))
          username    = T.takeWhile (/='!') . T.tail
          special     = find (($ s) . specialCond) (botSpecials b)
          isEmpty     = "PRIVMSG" `T.isInfixOf` s && T.all isSpace (clean s)
          isMode      = "MODE" `T.isInfixOf` s
          isPing      = "PING" `T.isInfixOf` s

eval :: [T.Text] -> T.Text -> Bot -> IO ()
eval [] _ b = privmsg b "I require a command."
eval s n b  =
    let comms      = botCommands b
        comm       = M.lookup (T.map toLower $ head s) (botCommands b)
    in maybe notFound respond comm
  where notFound = privmsg b $ "Command not found: " `T.append` head s
        respond c 
          | correctNumArgs (commandNumArgs c) (length (tail s)) = 
            commandFunc c (tail s) n b
          | otherwise                                           =
            privmsg b $ T.concat 
              [ "Incorrect number of arguments to command "
              , commandName c
              , " (expected "
              , showNumArgs (commandNumArgs c)
              , ", got "
              , T.pack . show . length $ tail s
              , ")"
              ]
                           
correctNumArgs :: (Int, Maybe Int) -> Int -> Bool
correctNumArgs (x, Nothing) n = n >= x
correctNumArgs (x, Just y) n  = n >= x && n <= y

showNumArgs :: (Int, Maybe Int) -> T.Text
showNumArgs (0, Nothing) = "any number of arguments"
showNumArgs (x, Nothing) = T.pack (show x) `T.append` " or more arguments"
showNumArgs (x, Just y)
    | x /= y    = T.concat ["any number of arguments between ", T.pack (show x), " and ", T.pack (show y)]
    | otherwise = T.concat [ "exactly ", T.pack (show x)
                           , if x == 1 then " argument" else "arguments" 
                           ]
          
              
