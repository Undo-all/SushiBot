{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bot 
import Data.Char
import System.Random
import System.Directory
import Text.HTML.Scalpel
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Process (readProcess)

specialLove :: Special
specialLove = 
  Special
    (\xs -> "i love you sushibot" `T.isInfixOf` T.filter (not . (`elem` ['.','!','?',',',';'])) (T.toLower xs))
    (\_ b -> privmsg b "Coming from anyone else, that'd be flattering.")

commandInfo :: Command
commandInfo =
  Command
    "info"
    "show general info about SushiBot"
    (0, Just 0)
    (\[] _ b -> 
        privmsg b
          "SushiBot is a bot written in Haskell, by the great almightly god-like being that is undoall. It's pretty shit.")

commandHelp :: Command
commandHelp =
  Command
    "help"
    "show list of commands"
    (0, Just 0)
    (\[] _ b -> 
        mapM_ (privmsg b . (\(Command n d _ _) -> T.concat [n, " - ", d])) (botCommands b))

commandSource :: Command
commandSource = 
  Command
      "source"
      "get source code"
      (0, Just 0)
      (\[] _ b -> 
          privmsg b "The source code for SushiBot can be found at https://github.com/Undo-all/SushiBot")

commandSlap :: Command
commandSlap = 
  Command
      "slap"
      "slap someone with a large trout"
      (1, Just 1)
      (\[s] _ b ->
          action b $ T.concat ["slaps ", s, " around a bit with a large trout."])

commandMix :: Command
commandMix = 
  Command
      "mix"
      "mix two or more drinks"
      (2, Nothing)
      mix
  where mix [x,y] n b =
            action b $ T.concat ["skillfully mixes "
                                , x
                                , " and "
                                , y
                                , " and slides it to "
                                , n
                                , "."
                                ]
        mix xs n b =
            action b $ T.concat [ "skillfully mixes "
                                , T.intercalate ", " (init xs)
                                , "and"
                                , last xs
                                , " and slides it to "
                                , n
                                , "."
                                ]

commandTime :: Command
commandTime = 
  Command
    "time"
    "get the time from the terminal that SushiBot is running on"
    (0, Just 0)
    (\[] _ b -> 
        readProcess "date" [] [] >>= (\s -> privmsg b $ T.concat
                                              [ "It is "
                                              , T.pack (init s) 
                                              , " but you could've just" 
                                              , " looked at your own clock."
                                              , " Dumbass."
                                              ]))

commandKill :: Command
commandKill =
  Command
      "kill"
      "kill someone"
      (1, Just 1)
      kill
  where kill [n] _ b 
            | ln == bn || ln `elem` fuckYous = 
                  privmsg b "Fuck you too, buddy."
            where ln = T.toLower n
                  bn = T.toLower (botName b)
                  fuckYous = ["yourself", "self", "itself", "his self", "her self", "bot"]
        kill [n] u b
          | u == n || n == "me" = privmsg b "I would link to a suicide \
                                            \hotline, but considering the \
                                            \fact that you're trying to use \
                                            \an IRC bot to kill yourself, \
                                            \I'm not too worried."
        kill [n] _ b = privmsg b $ T.concat 
                         [ "If a shitty IRC bot could kill " 
                         , n
                         , ", then someone would've already done it by now."
                         ]

commandFortune :: Command
commandFortune = 
  Command
    "fortune"
    "a direct call to the Unix command \"fortune\""
    (0, Just 0)
    (\[] _ b -> readProcess "fortune" [] [] >>= mapM_ (privmsg b . T.pack) . lines . init)

commandLewd :: Command
commandLewd =
  Command
    "lewd"
    "lewd a senpai :3"
    (1, Just 1)
    lewd 
  where lewd ["me"] u b = action b $ T.concat ["refuses to enter ", u, "'s magical realm"]
        lewd [n] _ b    = action b $ T.concat ["refuses to enter ", n, "'s magical realm"]

commandSend :: Command
commandSend =
  Command
      "send"
      "send several things, such as help, and nudes"
      (1, Just 1)
      commSend
  where commSend ["nudes"] u b = randomNude u >>= privmsg b . T.pack
        commSend [what] _ b 
            | what == "hugs" || what == "cuddles" = choice hugs >>= privmsg b
            where hugs = ["(>^_^)>", "<(^o^<)", "＼(^o^)／"]
        commSend _ _ b = privmsg b "I'm unfortunately too stupid to know how to send that. Blame it on my retarded creator."
        choice l = fmap (l !!) (randomRIO (0, length l - 1))

randomNude :: T.Text -> IO String
randomNude u = do
    np <- fromMaybe 0 <$> scrapeURL (root ++ "0") numPages
    let n = if np `div` 42 > 50 then 50 * 42 else np
    page <- fmap ((root++) . show) (randomRIO (0, n) :: IO Int)
    xs <- scrapeURL page images
    case xs of
      Nothing -> return "There aren't any images with your preferances."
      Just xs -> do img <- choice xs 
                    return $ "http://gelbooru.com/" ++ img
  where root :: String 
        root
            | u == "team_malice" =
              "http://gelbooru.com/index.php?page=post&s=list\
              \&tags=score%3A%3E%3D10+female+nude+solo+touhou+-gay+-futanari&pid="
            | u == "undoall" =
              "http://gelbooru.com/index.php?page=post&s=list\
              \&tags=score%3a%3E%3d10+rating%3aexplicit&pid="
            | u == "steenuil" =
              "http://gelbooru.com/index.php?page=post&s=list\
              \&tags=score%3A%3E%3D10+futanari&pid=" 
            | u == "Ni-chan" =
              "http://gelbooru.com/index.php?page=post&s=list\
              \&tags=guro&pid="
            | u == "itamae" || u == "itamae-fone" || u == "itamae-ssh" =
              "http://gelbooru.com/index.php?page=post&s=list\
              \&tags=score%3A%3E%3D10+feet+female+-futanari+-gay&pid=" 
            | u == "invoker" || u == "shiina-san" =
              "http://gelbooru.com/index.php?page=post&s=list\
              \&tags=score%3A%3E%3D8+female+-yaoi+-gay&pid="
            | otherwise = 
              "http://gelbooru.com/index.php?page=post&s=list\
              \&tags=score%3A%3E%3D10+female+nude+-gay+-futanari&pid="
        choice x = fmap (x !!) (randomRIO (0, length x - 1))
        numPages = do
            x <- attr ("href" :: String) $ ("a" :: String) @: [("alt" :: String) @= "last page"]
            let n = reverse . takeWhile isDigit . reverse $ x
            return (read n :: Int)
        images = do
            let link = attr ("href" :: String) $ ("a" :: String) @: []
            chroots (("span" :: String) @: [hasClass ("thumb" :: String)]) link

commandFlip :: Command
commandFlip =
  Command
    "flip"
    "flip a coin"
    (0, Just 0)
    (\[] _ b -> randomRIO (0, 1) >>= privmsg b . (T.append "Flipped ") . (["heads", "tails"] !!))

menu :: IO (M.Map T.Text [T.Text])
menu = fmap (parse M.empty . T.lines) file
  where file             = T.readFile "menu.txt"
        parse m [""]     = m
        parse m ("":f)   = parse m f
        parse m (name:f) = parse (M.insert name sushi m) (dropWhile (/= T.empty) f)
          where sushi = takeWhile (/= T.empty) f

commandMenu :: Command
commandMenu = 
  Command
    "menu"
    "list sushi available for order"
    (0, Just 0)
    (\[] _ b -> menu >>= privmsg b . T.intercalate ", " . M.keys)

commandOrder :: Command
commandOrder = 
  Command
    "order"
    "order a sushi off the menu"
    (1, Just 1)
    draw
  where draw [sushi] _ b = menu >>= mapM_ (privmsg b) . fromMaybe ["I am not familiar with that kind of sushi."] . M.lookup sushi

commandShoot :: Command
commandShoot =
  Command
    "shoot"
    "shoot someone"
    (1, Just 1)
    (\[n] _ b -> action b $ "shoots " `T.append` n)

commandLewdBot :: Command
commandLewdBot = 
  Command
    "lewdbot"
    "RIP"
    (0, Nothing)
    (\_ _ b -> privmsg b "She's dead now :)")

commandWeebMedia :: Command
commandWeebMedia = 
  Command
    "weebmedia"
    "get a random anime or manga off of ANN"
    (0, Nothing)
    (\_ _ b -> do n <- T.pack . show <$> (randomRIO (1, 17824) :: IO Int)
                  privmsg b $ T.append root n)
  where root = "http://www.animenewsnetwork.com/encyclopedia/anime.php?id="

main :: IO ()
main = 
  connectBot "irc.sushigirl.tokyo" 6667 "SushiBot" "SushiBot" "#lounge" True
    [ commandInfo
    , commandHelp
    , commandSlap
    , commandMix
    , commandTime
    , commandKill
    , commandFortune
    , commandSource
    , commandLewd
    , commandSend
    , commandFlip
    , commandMenu
    , commandOrder
    , commandShoot
    , commandLewdBot 
    , commandWeebMedia
    ]

    [ specialLove ]

