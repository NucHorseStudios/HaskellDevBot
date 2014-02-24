{-# LANGUAGE ScopedTypeVariables #-}
module HaskellBot where

import Data.List
import Network
import System.IO
import System.IO.Unsafe
import System.Exit
import System.Time
import System.Locale
import System.Timeout
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad
import Control.Exception 
import Control.Concurrent
import Control.Concurrent.STM
import Text.Printf
import Prelude hiding (catch)
import Data.List
import Data.List.Split
import Data.List.Utils
import Data.Char
import Data.Time
import Data.Maybe
import Data.Time.Parse
import Data.Convertible (convert)
import System.Posix.Types (EpochTime(..))
import Data.Time.Clock (UTCTime(..))
import FeedTypes
import FeedParser
import Database.HDBC
import Database.HDBC.Sqlite3 
import BotDataTypes
import BotDB
import BotConfig

botCommands = 
    [
        BotCommand {
            cmdName     = "!quit",
            cmd         = (\t u c -> do 
                                        admin       <- isAdmin u
                                        
                                        if admin
                                        then (write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)) 
                                        else return ())
        },

        BotCommand {
            cmdName     = "!credits",
            cmd         = (\t u c -> (privmsgTo c "Programmed by DrAwesomeClaws"))
        },

        BotCommand {
            cmdName     = "!say",
            cmd         = (\t u c -> do
                                        admin       <- isAdmin u
                                        
                                        if admin 
                                        then privmsgTo c (drop 4 (clean t)) 
                                        else return ())
        },

        BotCommand {
            cmdName     = "!uptime",
            cmd         = (\t u c -> (uptime >>= (privmsgTo c)))
        },

        BotCommand {
            cmdName     = "!help",
            cmd         = (\t u c -> privmsgTo c "Commands: !help !uptime \
                                                 \ !credits !source !say \
                                                 \ !quit !listfeeds")
        },

        BotCommand {
            cmdName     = "!source",
            cmd         = (\t u c -> privmsgTo c "https://github.com/NucHorseStudios/HaskellDevBot")
        },

        BotCommand {
            cmdName     = "!admin",
            cmd         = (\t u c
                            -> do
                                admin <- isAdmin u

                                let msg  = words . drop 7 . clean $ t
                                
                                let com  =  if null msg
                                            then Nothing 
                                            else Just $ msg !! 0

                                let user =  if null msg || length msg < 2
                                            then Nothing 
                                            else Just $ msg !! 1

                                case (user, com) of
                                     (Just n, Just c) -> 
                                        case c of 
                                            "add"    -> addAdminUser n
                                            "remove" -> removeAdminUser n
                                            _        -> io $ return ()
                                     (_,_)           -> io $ return ())
        },

        BotCommand {
            cmdName     = "!notify",
            cmd         = (\t u c 
                            -> do 
                                let msg = words . drop 7 . clean $ t

                                let com =   if null msg
                                            then Nothing
                                            else Just $ msg !! 0

                                let feedn = if null msg || length msg < 2
                                            then Nothing
                                            else Just $ msg !! 1

                                io $ putStrLn "Notify"

                                case (com, feedn) of
                                    (Just c, Just fn) -> 
                                        case c of 
                                            "on"    -> startNotify u fn
                                            "stop"  -> stopNotify  u fn
                                            _       -> io $ return ()
                                    (_,_)           -> io $ return ())
        }
    ]


io :: IO a -> Net a
io = liftIO

main :: IO ()
main 
    = do 
        forever $ catch startBot onError 
    where
        onError :: IOException -> IO ()
        onError e = (threadDelay fiveSeconds) >> return ()

        fiveSeconds :: Int
        fiveSeconds = 5 * 1000000

startBot :: IO ()
startBot 
    = do 
        mv <- atomically $ newTVar ([] :: [String])
        bracket (connect mv) disconnect loop 
        where
            disconnect = hClose . socket
            loop st    = catch (runReaderT runBot st) (\(e :: IOException) -> return ()) 

connect :: TVar [String] -> IO Bot
connect mv
    = notify $ do
        h     <- connectTo server $ PortNumber $ fromIntegral port
        t     <- getClockTime
        dbh   <- connectDb "haskellbot.db"
        feeds <- getFeeds dbh

        hSetBuffering h NoBuffering
        forkIO $ dispatchMessages h 1000000 mv
        mapM ((spawnFeedProc dbh) h) feeds
        return (Bot h t mv dbh)
    
    where
        notify a = bracket_
            ((printf "Connecting to %s ... " server)  >> (hFlush stdout))
            (putStrLn "done.")
            a

        spawnFeedProc :: IConnection c => c -> Handle -> FeedSource -> IO (ThreadId)
        spawnFeedProc dbh h fs 
            = do
                fd <- atomically $ newTVar ([] :: [FeedData]) 

                forkIO $ updateFeed dbh h rt fs fd mv
            
            where
                rt :: Int
                rt = (feedRefreshTime fs) * 1000000
    
runBot :: Net ()
runBot 
    = do
        setNick     botNick
        setUser     (botNick++" 0 * :haskelldev bot")
        joinChannel ircChannel
        asks socket >>= listen 

listen :: Handle -> Net ()
listen h 
    = do
        s <- io $ timeout threeSeconds $ init `fmap` (hGetLine h)
        
        case s of 
            Nothing -> (io $ hPutStrLn h " ") >> listen h
            Just s  -> (eval s) >> (io $ putStrLn s) >> listen h
    where
        threeSeconds :: Int
        threeSeconds = 3 * 1000000

write :: String -> String -> Net ()
write s t 
    = do
        h <- asks socket
    
        io $ hPrintf h "%s %s\r\n" s t
        io $ printf    "> %s %s\n" s t

addMessage :: TVar [String] -> String-> STM ()
addMessage mv m
    = do
        msgs <- readTVar mv
        writeTVar mv (m : msgs)

popMessages :: TVar [String] -> STM (Maybe String)
popMessages mv
    = do
        m <- readTVar mv
        case m of
            [] -> return Nothing
            x  -> do 
                    (writeTVar mv (init x)) 
                    return $ Just (last x)

dispatchMessages :: Handle -> Int -> TVar [String] -> IO ()
dispatchMessages h d mv
    =  do  
        m <- atomically $ popMessages mv
        case m of
            Nothing ->   waitAndRecur
            Just x  ->   hPutStrLn h (msg x) >> putStrLn (msg x) >> waitAndRecur
        where
            msg :: String -> String
            msg x        = "PRIVMSG " ++ x 
            
            waitAndRecur :: IO ()
            waitAndRecur = threadDelay d >> dispatchMessages h d mv

eval :: String -> Net ThreadId
eval t 
    = do
        st <- ask
        if (ping t) then (io $ forkIO $ runReaderT (pong t) st)
        else case (getCommand botCmd) of
            Nothing  -> io $ forkIO $ runReaderT (return ()) st
            Just bc  -> io $ forkIO $ runReaderT ((cmd bc) t ircUser chan) st 
        where
            parseIrcCmd :: [String]
            parseIrcCmd = words t

            ircUser :: String
            ircUser = drop 1 (splitOn "!" (parseIrcCmd !! 0) !! 0)
            
            chan :: String
            chan = if ((parseIrcCmd !! 2) == botNick) then ircUser else (parseIrcCmd !! 2) 
            
            botCmd :: String
            botCmd = if null (words (clean t)) then "" else (words (clean t)) !! 0

getCommand :: String -> Maybe BotCommand
getCommand t 
    = do
        case (filter (isCmd t) botCommands) of 
            []      -> Nothing 
            (x:xs)  -> Just x 
    where 
        isCmd t bc = if (t == (cmdName bc)) then True else False


isIdentified :: String -> Net Bool
isIdentified u
    = do
        h  <- asks socket 
        r  <- io $ timeout fiveSeconds (waitForResp h)
        case r of 
            Nothing  -> return False
            Just x   -> return x 
    where 
        fiveSeconds = 5 * 1000000
        waitForResp h
            = do
                s <- init `fmap` (hGetLine h) 

                if (u ++ " ACC 3") `isPrefixOf` (clean s)
                then return True 
                else waitForResp h

clean :: String -> String
clean = drop 1 . unwords . drop 3 . words

isAdmin :: String -> Net Bool
isAdmin u 
    = do
        dbh <- asks db
        au  <- io $ getAccessGroupUsers dbh "admin"
        
        privmsgTo "NickServ" ("ACC " ++ u)
        identified <- isIdentified u
        
        case au of 
            Nothing -> return False
            Just au 
                -> case (filter isUser au) of
                    []  -> return False
                    [x] -> return identified
    
    where
        isUser :: User -> Bool
        isUser a = ((nick a) == u)

removeAdminUser :: String -> Net ()
removeAdminUser n
    = do
        dbh <- asks db
        g   <- io $ getGroupByName dbh "admin"
        au  <- io $ (getUser dbh n)
        
        if n == master 
        then io $ return ()
        else case au of
            Nothing -> io $ return ()
            Just u  -> io $ removeUserFromAccessGroup dbh u (fromJust g)

addAdminUser :: String -> Net ()
addAdminUser n
    = do
        dbh <- asks db
        g   <- io $ getGroupByName dbh "admin"
        au  <- io $ (addUser dbh n 0) 
        
        io $ addUserToAccessGroup dbh (fromJust au) (fromJust g)


stopNotify :: String -> String -> Net ()
stopNotify u fn 
    = do
        dbh <- asks db
        usr <- io $ getUser dbh u
        fd  <- io $ getFeed dbh (read fn :: Int) 

        case (usr, fd) of
            (Just usr, Just fd) -> io $ stopNotificationsFor dbh usr fd
            (_,_)               -> io $ return ()

startNotify :: String -> String -> Net ()
startNotify u fn 
    = do
        dbh <- asks db
        usr <- io $ getUser dbh u
        fd  <- io $ getFeed dbh (read fn :: Int) 

        case (usr, fd) of
            (Just usr, Just fd) -> io $ startNotificationsFor dbh usr fd
            (_,_)               -> io $ return ()

uptime :: Net String
uptime 
    = do
        now  <- io getClockTime
        zero <- asks startTime
        return . prettytime $ diffClockTimes now zero

prettytime :: TimeDiff -> String
prettytime td = 
    unwords $ map (uncurry (++) . first show) $
    if null diffs then [(0,"s")] else diffs
        where 
            merge (tot, acc) (sec, typ) = let (sec',tot') = divMod tot sec
                                          in (tot', (sec',typ):acc)
            metrics = [(86400, "d"), (3600, "h"), (60, "m"), (1, "s")]
            diffs   = filter ((/= 0) . fst) $ reverse $ snd $
                      foldl' merge (tdSec td, []) metrics

isWithin :: Int -> UTCTime -> UTCTime -> Bool
isWithin i a b = (fromEnum $  (utcTimeToEpochTime a) - (utcTimeToEpochTime b)) < i

utcTimeToEpochTime :: UTCTime -> EpochTime
utcTimeToEpochTime = convert

replaceFeedData :: TVar [FeedData] -> [FeedData] -> STM ([FeedData])
replaceFeedData ftv nd 
    = do
        od <- readTVar ftv
        writeTVar ftv nd
        return od

updateFeed :: IConnection c => c -> Handle -> Int -> FeedSource -> TVar [FeedData] -> TVar [String] -> IO ()
updateFeed dbh h d fs fd mv
    = do
        nd   <- getFeedData fs
        
        putStrLn $ "Updating Feed: " ++ (feedName fs)

        case nd of 
            Nothing -> putStrLn "Error getting data" >> waitAndRecur  
            Just d  -> 
                do  
                    od <- atomically $ replaceFeedData fd d

                    if null od 
                    then waitAndRecur
                    else (writeFeedData dbh h mv (getNewItems od d)) >> waitAndRecur

        where
            getNewItems :: [FeedData] -> [FeedData] -> [FeedData]
            getNewItems od nd = filter (notOlderThan 3600) (nd \\ od)
            
            notOlderThan :: Int -> FeedData -> Bool
            notOlderThan s a  = (isWithin s ct) (pubDateUTC a)

            ct :: UTCTime
            ct = (unsafePerformIO getCurrentTime)

            pubDateUTC :: (FeedData -> UTCTime)
            pubDateUTC = parseToUTC . feedItemPubDate 
        
            parseToUTC :: (String -> UTCTime)
            parseToUTC = (readTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z")
       
            waitAndRecur :: IO ()
            waitAndRecur = threadDelay d >> updateFeed dbh h d fs fd mv
                
writeFeedData :: IConnection c => c -> Handle -> TVar [String] -> [FeedData] -> IO [()]
writeFeedData dbh h mv f 
    = do
        writeItem `mapM` f
    where 
        writeItem x = do
                        usrs <- notifiedUsers dbh (feedItemSource x)
                        mapM (\u -> (sendTo (nick u) x)) usrs
                        atomically $ addMessage mv (m ircChannel x)
                        return ()

        m  w x      = w ++ " : " ++ name x ++ ": " ++ text x ++ " <" ++ url x ++ ">"
        sendTo w x  = atomically $ addMessage mv (m w x)
        name x      = (feedName (feedItemSource x))
        text x      = (feedItemTitle x)
        url  x      = take (39 + (length $ name x)) (feedItemUrl x) 

ping :: String -> Bool
ping x = "PING :" `isPrefixOf` x

pong :: String -> Net () 
pong x = write "PONG" (':' : drop 6 x)

setUser :: String -> Net ()
setUser u = write "USER" u

setNick :: String -> Net ()
setNick n = write "NICK" n

joinChannel :: String -> Net ()
joinChannel ch = write "JOIN" ch

privmsg :: String -> Net ()
privmsg text = privmsgTo ircChannel text

privmsgTo :: String -> String -> Net ()
privmsgTo to text 
    = do 
        mv <- asks messages
        io $ atomically $ addMessage mv (to ++ " :" ++ text)

