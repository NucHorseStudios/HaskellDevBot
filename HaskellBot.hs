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
import Database.HDBC.PostgreSQL 
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
            cmd         = (\t u c 
                            -> do
                                let defHelp =   privmsgTo u "Commands: !help !uptime !credits !source !say \
                                                            \ !quit !listfeeds !admin !notify !lastseen" >>
                                                privmsgTo u "!help <command> for help with individual commands" >>
                                                privmsgTo u "Exmaple: !help notify"

                                let lsHelp     =    privmsgTo u "!lastseen <nick>" >>
                                                    privmsgTo u "Example: !lastseen DrAwesomeClaws"

                                let notifyHelp =    privmsgTo u "Notify on new post." >>
                                                    privmsgTo u "!notify on <Feed id>  -- \
                                                                \ Starts notifications for a feed" >>
                                                    privmsgTo u "!notify off <Feed id> -- \
                                                                \ Stops notifications for a feed" >> 
                                                    privmsgTo u "Example: !notify on 2" >>
                                                    privmsgTo u "You can get feed ids from !listfeeds"
                                
                                let adminHelp =     privmsgTo u "Admin Commands" >>
                                                    privmsgTo u "Must already be an administrator" >>
                                                    privmsgTo u "!admin add <nick>" >>
                                                    privmsgTo u "!admin remove <nick>" >>
                                                    privmsgTo u "Example: !admin add DrAwesomeClaws"
                                        
                                let msg     = words . drop 6 . clean $ t
                                
                                if null msg
                                then defHelp
                                else case (msg !! 0) of
                                    "admin"     -> adminHelp
                                    "!admin"    -> adminHelp

                                    "notify"    -> notifyHelp
                                    "!notify"   -> notifyHelp

                                    "lastseen"  -> lsHelp
                                    "!lastseen" -> lsHelp

                                    _           ->  defHelp)

        },

        BotCommand {
            cmdName     = "!listfeeds",
            cmd         = (\t u c -> listfeeds c) 
        },

        BotCommand {
            cmdName     = "!source",
            cmd         = (\t u c -> privmsgTo c "https://github.com/NucHorseStudios/HaskellDevBot")
        },

        BotCommand {
            cmdName     = "!text",
            cmd         = (\t u c 
                            -> do
                                dbh <- asks db
                                let msg = words . drop 6 . clean $ t
                                let pid = if null msg then Nothing else Just $ msg !! 0
                                
                                
                                case pid of 
                                    Just id 
                                        -> do
                                            fd <- io $ getFeedDataById dbh (read id :: Int)
                                            case fd of 
                                                Just fd -> msgChunks u (feedItemText fd)
                                                _       -> io $ return ()
                                    _       -> io $ return ())
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
                                if not admin 
                                then return ()
                                else case (user, com) of
                                            (Just n, Just c) -> 
                                                case c of 
                                                    "add"    -> addAdminUser n
                                                    "remove" -> removeAdminUser n
                                                    _        -> io $ return ()
                                            (_,_)            -> io $ return ())
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
                                            "off"   -> stopNotify  u fn
                                            _       -> io $ return ()
                                    (_,_)           -> io $ return ())
        },

        BotCommand {
            cmdName     = "!lastseen",
            cmd         = (\t u c 
                            -> do
                                let msg = words . drop 10 . clean $ t

                                let n   =   if null msg
                                            then Nothing
                                            else Just $ msg !! 0
                                case n of 
                                    Just nck -> (lastseen nck) >>= (privmsgTo c)
                                    _        -> io $ return ())

        }
    ]

msgChunks :: String -> String -> Net ()
msgChunks c [] = return ()
msgChunks c x  = privmsgTo c (take 400 x)  >> msgChunks c (drop 400 x)

io :: IO a -> Net a
io = liftIO


main :: IO ()
main 
    = do 
        forever $ catch startBot onError 
    where
        onError :: IOException -> IO ()
        onError e   = (threadDelay fiveSeconds) >> return ()

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
        dbh   <- connectDb dbs
        feeds <- getFeeds dbh

        hSetBuffering h NoBuffering
        mapM (spawnFeedProc dbh) feeds
        forkIO $ dispatchFeedData dbh mv True  
        forkIO $ dispatchMessages h oneSecond mv

        return (Bot h t mv dbh)
    
    where
        oneSecond :: Int
        oneSecond = 1000000
        
        dbs :: String
        dbs = "host=localhost dbname=" ++ dbName ++ " user=" ++ (map toLower botNick) ++ " password="

        notify :: IO Bot -> IO Bot
        notify a = bracket_
            ((printf "Connecting to %s ... " server)  >> (hFlush stdout))
            (putStrLn "done.")
            a

        spawnFeedProc :: IConnection c => c -> FeedSource -> IO (ThreadId)
        spawnFeedProc dbh fs 
            = do 
                forkIO $ updateFeed dbh rt fs 
            where
                rt :: Int
                rt = (feedRefreshTime fs) * oneSecond
    

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
        st  <- ask
        dbh <- asks db
        ct  <- io $ getCurrentTime
        
        io $ addUser_ dbh ircUser (fromEnum $ utcTimeToEpochTime ct)
        
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


listfeeds :: String -> Net ()
listfeeds c 
    = do
        dbh   <- asks db
        feeds <- io $ getFeeds dbh

        mapM showFeed feeds
        return ()

    where 
        showFeed f = privmsgTo c (msg f)
        msg f      = (show (feedId f)) ++ ": " ++ (feedName f) ++ " <" ++ (feedUrl f) ++ ">"


lastseen :: String -> Net String
lastseen n 
    = do
        dbh <- asks db
        usr <- io $ getUser dbh n

        case usr of 
            Nothing -> return $ "I don't know " ++ n
            Just u  
                -> do
                    now  <- io $ getClockTime
                    
                    return $ "I last saw " ++ n ++ " " ++ (prettytime $ diffClockTimes now lsCt) ++ " ago." 
                
                where 
                    lsCt = TOD (toInteger $ lastSeen u) 0

    where
        parseToUTC :: (String -> UTCTime)
        parseToUTC = (readTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z")


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
            (Just usr, Just fd) 
                -> do
                    io $ stopNotificationsFor dbh usr fd
                    privmsgTo u ("Stopping notifications for feed: " ++ (feedName fd))
            _   -> io $ return ()


startNotify :: String -> String -> Net ()
startNotify u fn 
    = do
        dbh <- asks db
        usr <- io $ getUser dbh u
        fd  <- io $ getFeed dbh (read fn :: Int) 

        case (usr, fd) of
            (Just usr, Just fd) 
                -> do
                    r <- io $ getNotificationStatus dbh usr fd

                    case r of 
                        False -> (io $ startNotificationsFor dbh usr fd) >>
                                 (privmsgTo u ("Starting notifications for feed: " ++ (feedName fd)))
                        True  -> (privmsgTo u ("You are already being notified for that feed."))
            
            _   -> io $ return ()


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


utcTimeToEpochTime :: UTCTime -> EpochTime
utcTimeToEpochTime = convert


updateFeed :: IConnection c => c -> Int -> FeedSource -> IO ()
updateFeed dbh d fs 
    = do
        nd   <- getFeedData fs
        
        putStrLn $ "Updating Feed: " ++ (feedName fs)

        case nd of 
            Nothing  -> putStrLn "Error getting data" 
            Just dt  -> addFeedData dbh dt 
        
        waitAndRecur

        where
            waitAndRecur :: IO ()
            waitAndRecur = threadDelay d >> updateFeed dbh d fs


dispatchFeedData :: IConnection c => c -> TVar [String] -> Bool -> IO ()
dispatchFeedData dbh mv firstRun
    = do
        feeds <- getFeeds dbh

        threadDelay tenSeconds
        mapM (dispatch firstRun) feeds
        dispatchFeedData dbh mv False

    where
        dispatch fr fs 
            = do
                fd <- getNewFeedData dbh fs
                ct <- getCurrentTime
                
                setDataDispatched dbh fd
                if fr then return [()] else writeFeedData dbh mv (filter (notOlderThan 3600 ct) fd) 
        
        notOlderThan s ct d = ((epoch ct) - (postEpoch d)) < s 
        postEpoch d         = epoch (localTimeToUTC utc  (rddtToLt d))
        epoch               = fromEnum . utcTimeToEpochTime 
        rddtToLt d          = fst $ fromJust $ strptime "%a, %Od %b %Y %OH:%OM:%OS %z" (feedItemPubDate d)
        tenSeconds          = 10 * 1000000


writeFeedData :: IConnection c => c -> TVar [String] -> [FeedData] -> IO [()]
writeFeedData dbh mv f 
    = do
        writeItem `mapM` f
    where 
        writeItem x 
            = do
                usrs <- notifiedUsers dbh (feedItemSource x)

                mapM (\u -> (sendTo (nick u) x)) usrs
                sendTo ircChannel x

        m  w x      = w ++ " : " ++ name x ++ ": " ++ text x ++ " <" ++ url x ++ ">" ++ " [" ++ id x ++ "]"
        sendTo w x  = atomically $ addMessage mv (m w x)
        name x      = (feedName (feedItemSource x))
        text x      = (feedItemTitle x)
        url  x      = take (39 + (length $ name x)) (feedItemUrl x) 
        id   x      = (show (feedItemId x))


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

