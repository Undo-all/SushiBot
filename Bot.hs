module Bot where

import Network
import System.IO hiding (hPutStr, hPutStrLn)
import System.IO.UTF8 (hPutStr, hPutStrLn)
import Data.List
import Data.Maybe
import Control.Monad (when)
import Data.Char
import Control.Concurrent

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

send :: Bool -> Handle -> String -> IO ()
send l h s = do
    hPutStr h s
    when l $ print s

privmsg :: Bot -> String -> IO ()
privmsg b s = hPutStr (botHandle b) $ "PRIVMSG " ++ botChannel b ++ " :" ++ s ++ "\n"

action :: Bot -> String -> IO ()
action b s = hPutStr (botHandle b) $ "PRIVMSG " ++ botChannel b ++ " :\0001ACTION " ++ s ++ "\0001\n"

connectBot :: String -> Int -> String -> String -> String -> Bool -> [Command] -> [Special] -> IO ()
connectBot server port nick name chan logging comms specs =
    do h <- connectTo server (PortNumber (fromIntegral port))
       hSetBuffering h NoBuffering
    
       send logging h $ "USER " ++ nick ++ " " ++ nick ++ " " ++ nick ++ " :" ++ name ++ "\n"
       send logging h $ "NICK " ++ nick ++ "\n"

       let b = Bot server port nick name chan logging comms specs h 

       botLoop h b

botLoop :: Handle -> Bot -> IO ()
botLoop h b = do s <- hGetLine h
                 when (botLogging b) $ print s 
                 handleData s h b
                 botLoop h b

handleData :: String -> Handle -> Bot -> IO ()
handleData s h b
    | (take 4 s) == "PING" = send (botLogging b) h ("PONG " ++ (drop 4 s) ++ "\n")
    | "MODE" `isInfixOf` s = send (botLogging b) h ("JOIN " ++ (botChannel b) ++ "\n")
    | "PRIVMSG" `isInfixOf` s && all isSpace (clean s) = return ()
    | isJust special = maybe (error "nope.") (\x -> specialFunc x (clean s) b) special
    | isCommand s    = eval ((\(x:xs) -> (tail x) : xs) $ space $ words (clean s)) (username s) h b
    | otherwise      = return ()
    where isCommand m = "PRIVMSG" `isInfixOf` m &&
                        head (clean m) == '!'
          clean       = tail . dropWhile (/= ':') . tail
          space       = map (map (\x -> if x == '_' then ' ' else x))
          username    = takeWhile (/='!') . tail
          special     = find (($ s) . specialCond) (botSpecials b)

eval [] _ _ b = privmsg b "I require a command."
eval s n _ b  = let comms = botCommands b
                    comm  = lookup (map toLower $ head s) $ 
                            map (\c@(Command cn _ _ _) -> (cn,c)) comms
                in maybe 
                     (privmsg b $ "Command not found: " ++ head s)
                     (\c -> if correctNumArgs (commandNumArgs c) (length (tail s))
                              then commandFunc c (tail s) n b 
                              else privmsg b $ 
                                "Incorrect number of arguments to command " ++
                                 commandName c ++ " (expected " ++ 
                                 showNumArgs (commandNumArgs c) ++
                                 ", got " ++ show (length $ tail s) ++ ")")
                     comm

correctNumArgs :: (Int, Maybe Int) -> Int -> Bool
correctNumArgs (x, Nothing) n = n >= x
correctNumArgs (x, Just y) n  = n >= x && n <= y

showNumArgs :: (Int, Maybe Int) -> String
showNumArgs (0, Nothing) = "any number of arguments"
showNumArgs (x, Nothing) = show x ++ " or more arguments"
showNumArgs (x, Just y)
    | x /= y    = "any number of arguments between " ++ show x ++ " and " ++ show y
    | otherwise = "exactly " ++ show x ++ " arguments"
          
               
