{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bot 
import System.Random
import Text.HTML.Scalpel
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Process (readProcess)

specialLove = 
    Special
        (\xs -> "i love you sushibot" `T.isInfixOf` T.filter (not . (`elem` ['.','!','?',',',';'])) (T.toLower xs))
        (\_ b -> privmsg b "Coming from anyone else, that'd be flattering.")

commandInfo =
    Command
        "info"
        "show general info about SushiBot"
        (0, Just 0)
        (\[] _ b -> 
            privmsg b
                "SushiBot is a bot written in Haskell, by the great almightly god-like being that is undoall. It's pretty shit.")

commandHelp =
    Command
        "help"
        "show list of commands"
        (0, Just 0)
        (\[] _ b -> 
            mapM_ (privmsg b . (\(Command n d _ _) -> T.concat [n, " - ", d])) (botCommands b))

commandSource = 
    Command
        "source"
        "get source code"
        (0, Just 0)
        (\[] _ b -> 
            privmsg b "The source code for SushiBot can be found at https://github.com/Undo-all/SushiBot")

commandSlap = 
    Command
        "slap"
        "slap someone with a large trout"
        (1, Just 1)
        (\[s] _ b ->
                action b $ T.concat ["slaps ", s, " around a bit with a large trout."])

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

commandFortune = 
    Command
        "fortune"
        "a direct call to the Unix command \"fortune\""
        (0, Just 0)
        (\[] _ b -> readProcess "fortune" [] [] >>= mapM_ (privmsg b . T.pack) . lines . init)

commandLewd =
    Command
        "lewd"
        "lewd a senpai :3"
        (1, Just 1)
        lewd 
    where lewd ["me"] u b = action b $ T.concat ["refuses to enter ", u, "'s magical realm"]
          lewd [n] _ b    = action b $ T.concat ["refuses to enter ", n, "'s magical realm"]

commandSend =
    Command
        "send"
        "send several things, such as help, and nudes"
        (1, Just 1)
        commSend
    where commSend ["nudes"] u b
              | u == "undoall" = randomNude False >>= privmsg b . T.pack
              | otherwise      = randomNude True >>= privmsg b . T.pack
          commSend [what] _ b 
              | what == "hugs" || what == "cuddles" = choice hugs >>= privmsg b
              where hugs = ["(>^_^)>", "<(^o^<)", "＼(^o^)／"]
          commSend _ _ b = privmsg b "I'm unfortunately too stupid to know how to send that. Blame it on my retarded creator."
          choice l = fmap (l !!) (randomRIO (0, length l - 1))

-- aod = afraid of dicks. Toggles if dicks/gay shit
-- will show up in result.
randomNude :: Bool -> IO String
randomNude aod = do
    page <- fmap ((root++) . show) (randomRIO (0, 10000) :: IO Int)
    xs <- scrapeURL page images
    case xs of
      Nothing -> error "Something in finding nudes has gone horribly wrong."
      Just xs -> do img <- choice xs 
                    return $ "http://gelbooru.com/" ++ img
  where root :: String 
        root
          | aod =
            "http://gelbooru.com/index.php?page=post&s=list\
            \&tags=score%3A%3E%3D10+female+nude+solo+-gay+-futanari&pid="
          | otherwise =
            "http://gelbooru.com/index.php?page=post&s=list\
            \&tags=score%3A%3E%3D10+rating%3Aexplicit+&pid="
        choice x = fmap (x !!) (randomRIO (0, length x - 1))
        images = do
            let link = do x <- attr ("href" :: String) $ ("a" :: String) @: []
                          return x
            chroots (("span" :: String) @: [hasClass ("thumb" :: String)]) link

commandFlip =
    Command
        "flip"
        "flip a coin"
        (0, Just 0)
        (\[] _ b -> randomRIO (0, 1) >>= privmsg b . (T.append "Flipped ") . (["heads", "tails"] !!))

sushis = fmap (parse M.empty . T.lines) file
    where file             = T.readFile "sushis.txt"
          parse m [""]     = m
          parse m ("":f)   = parse m f
          parse m (name:f) = parse (M.insert name sushi m) (dropWhile (/= T.empty) f)
              where sushi = takeWhile (/= T.empty) f

commandMenu = 
    Command
        "menu"
        "list sushi available for order"
        (0, Just 0)
        (\[] _ b -> sushis >>= privmsg b . T.intercalate ", " . M.keys)

commandOrder = 
    Command
        "order"
        "order a sushi off the menu"
        (1, Just 1)
        draw
    where draw [sushi] _ b = sushis >>= mapM_ (privmsg b) . fromMaybe ["I am not familiar with that kind of sushi."] . M.lookup sushi

commandShoot =
    Command
        "shoot"
        "shoot someone"
        (1, Just 1)
        (\[n] _ b -> action b $ "shoots " `T.append` n)

commandLewdBot = 
    Command
        "lewdbot"
        "RIP"
        (0, Nothing)
        (\_ _ b -> privmsg b "She's dead now :)")

commandWeebMedia = 
    Command
        "weebmedia"
        "get a random anime or manga off of ANN"
        (0, Nothing)
        (\_ _ b -> do n <- T.pack . show <$> (randomRIO (1, 17824) :: IO Int)
                      privmsg b $ T.append root n)
  where root = "http://www.animenewsnetwork.com/encyclopedia/anime.php?id="

commandSuggest =
    Command
        "suggest"
        "make a suggestion; it will be added to a suggestions file"
        (0, Nothing)
        suggest
  where suggest xs u b = do T.writeFile "suggestions.log" (T.concat [u, ": ", T.unwords xs, "\n"])
                            privmsg b "Suggestion added."

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
           , commandSuggest
           ]

           [ specialLove ]

