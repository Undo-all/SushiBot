module Bot 
( Command(..)
, Special(..)
, Bot(..)
, connectBot
, privmsg
, action
) where

import Network
import Data.List
import Data.Char
import Data.Maybe
import Control.Concurrent
import Control.Monad (when)
import System.IO.UTF8 (hPutStr, hPutStrLn)
import System.IO hiding (hPutStr, hPutStrLn)

data Command = Command 
               { commandName :: String
               , commandDesc :: String
               , commandNumArgs :: (Int, Maybe Int)
               , commandFunc :: [String] -> String -> Bot -> IO ()
               }

data Special = Special
               { specialCond :: String -> Bool
               , specialFunc :: String -> Bot -> IO ()
               }  

data Bot = Bot
           { botServer :: String
           , botPort :: Int
           , botNick :: String
           , botName :: String
           , botChannel :: String
           , botLogging :: Bool
           , botCommands :: [Command]
           , botSpecials :: [Special]
           , botHandle :: Handle
           }

privmsg :: Bot -> String -> IO ()
privmsg b s = hPutStr (botHandle b) $ "PRIVMSG " ++ botChannel b ++ " :" ++ s ++ "\n"

action :: Bot -> String -> IO ()
action b s = hPutStr (botHandle b) $ "PRIVMSG " ++ botChannel b ++ " :\0001ACTION " ++ s ++ "\0001\n"

connectBot :: String -> Int -> String -> String -> String -> 
              Bool -> [Command] -> [Special] -> IO ()
connectBot server port nick name chan logging comms specs =
    do h <- connectTo server (PortNumber (fromIntegral port))
       hSetBuffering h NoBuffering
    
       hPutStr h $ "USER " ++ nick ++ " " ++ nick ++ " " ++ nick ++ " :" ++ name ++ "\n"
       hPutStr h $ "NICK " ++ nick ++ "\n"

       let b = Bot server port nick name chan logging comms specs h 

       botLoop h b

botLoop :: Handle -> Bot -> IO ()
botLoop h b = do s <- hGetLine h
                 when (botLogging b) $ print s 
                 handleData s h b
                 botLoop h b

handleData :: String -> Handle -> Bot -> IO ()
handleData s h b
    | isPing         = hPutStr (botHandle b) ("PONG " ++ drop 4 s ++ "\n")
    | isMode         = hPutStr (botHandle b) ("JOIN " ++ botChannel b ++ "\n")
    | isEmpty        = return ()
    | isJust special = maybe (error "nope.") (\x -> specialFunc x (clean s) b) special
    | isCommand s    = eval ((\(x:xs) -> tail x : xs) $ space $ words (clean s)) (username s) b
    | otherwise      = return ()
    where isCommand m = "PRIVMSG" `isInfixOf` m &&
                        head (clean m) == '!'
          clean       = tail . dropWhile (/= ':') . tail
          space       = map (map (\x -> if x == '_' then ' ' else x))
          username    = takeWhile (/='!') . tail
          special     = find (($ s) . specialCond) (botSpecials b)
          isEmpty       = "PRIVMSG" `isInfixOf` s && all isSpace (clean s)
          isMode        = "MODE" `isInfixOf` s
          isPing        = "PING" `isInfixOf` s

eval :: [String] -> String -> Bot -> IO ()
eval [] _ b = privmsg b "I require a command."
eval s n b  =
    let comms      = botCommands b
        comm       = lookup (map toLower $ head s) $ 
                     map (\c@(Command cn _ _ _) -> (cn,c)) comms
    in maybe notFound respond comm
  where notFound  = privmsg b $ "Command not found: " ++ head s
        respond c 
          | correctNumArgs (commandNumArgs c) (length (tail s)) = 
            commandFunc c (tail s) n b
          | otherwise                                           =
            privmsg b $ concat 
              [ "Incorrect number of arguments to command "
              , commandName c
              , "(expected "
              , showNumArgs (commandNumArgs c)
              , ", got "
              , show $ length (tail s)
              ]
                           
correctNumArgs :: (Int, Maybe Int) -> Int -> Bool
correctNumArgs (x, Nothing) n = n >= x
correctNumArgs (x, Just y) n  = n >= x && n <= y

showNumArgs :: (Int, Maybe Int) -> String
showNumArgs (0, Nothing) = "any number of arguments"
showNumArgs (x, Nothing) = show x ++ " or more arguments"
showNumArgs (x, Just y)
    | x /= y    = "any number of arguments between " ++ show x ++ " and " ++ show y
    | otherwise = "exactly " ++ show x ++ " arguments"
          
               
