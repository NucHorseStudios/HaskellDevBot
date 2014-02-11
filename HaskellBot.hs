{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import Network
import System.IO
import System.IO.Unsafe
import System.Exit
import System.Time
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad
import Control.Exception 
import Control.Concurrent
import Text.Printf
import Prelude hiding (catch)
import Data.List
import Data.List.Split
import Data.List.Utils
import Data.Char
import Data.Time
import Data.Time.Parse
import Data.Convertible (convert)
import System.Posix.Types (EpochTime(..))
import Data.Time.Clock (UTCTime(..))
import FeedTypes
import FeedParser

server      = "irc.freenode.org"
port        = 6667
ircChannel  = "##hb-testing"
nick        = "GamedevBotT"
master      = "DrAwesomeClaws"
admins      = master : ["DrAwesomeClaws_"]
feeds       =   [
                    FeedSource { 
                        feedName        = "r/gamedev", 
                        feedRefreshTime = 30,
                        feedUrl         = "http://www.reddit.com/r/gamedev/new/.rss"
                    },
                    FeedSource {
                        feedName        = "r/truegamedev",
                        feedRefreshTime = 30, 
                        feedUrl         = "http://www.reddit.com/r/truegamedev/new/.rss"
                    },
                    FeedSource {
                        feedName        = "r/gamedevbottesting",
                        feedRefreshTime = 10, 
                        feedUrl         = "http://www.reddit.com/r/gamedevbottesting/new/.rss"
                    }
                ]
data Bot = Bot { 
            socket    :: Handle,
            startTime :: ClockTime 
           }

type Net = ReaderT Bot IO

main :: IO ()
main =  bracket connect disconnect loop
        where
            disconnect = hClose . socket
            loop st    = catch (runReaderT run st) (\(e :: IOException) -> return ()) 

messages  = unsafePerformIO (newMVar [])

connect :: IO Bot
connect = notify $ do
        h <- connectTo server $ PortNumber $ fromIntegral port
        t <- getClockTime
        hSetBuffering h NoBuffering
        forkIO $ (dispatchMessages h 1000000)
        mapM (spawnFeedProc h) feeds
        return (Bot h t)
    where
        spawnFeedProc h fs = do
                            fm <- newEmptyMVar
                            forkIO $ updateFeed h ((feedRefreshTime fs) * 1000000) fs fm 
        notify a = bracket_
            (printf "Connecting to %s ... " server >> hFlush stdout)
            (putStrLn "done.")
            a

dispatchMessages h d = do
            m <- takeMVar messages  
            hPutStrLn h $ "PRIVMSG " ++ m
            putStrLn $ "PRIVMSG " ++ m
            threadDelay d
            dispatchMessages h d

updateFeed h d fs fm = do
    fd <- getFeedData fs
    lfd <- tryTakeMVar fm 
    putStrLn $ "Updating Feed: " ++ (feedName fs)
    case lfd of
        Nothing  -> waitAndRecur fd
        Just od  -> do
                        writeFeedData h $ (getNewItems od fd)
                        waitAndRecur fd
    where
        getNewItems od fd = ((notInOldData od) `filter` fd)
        notInOldData od a = unsafePerformIO $ do
                                new      <- (notOlderThan a 3600)
                                return $ (not $ a `elem` od) && new
        notOlderThan a  s = case gzt (cvtDate (feedItemPubDate a)) of
                                Left  _   -> return False
                                Right t   -> do
                                                putStrLn $ show $ utcTimeToEpochTime t
                                                return $ ageSeconds < s 
                                            where 
                                                cEpoch = (utcTimeToEpochTime $ unsafePerformIO getCurrentTime)
                                                pEpoch = (utcTimeToEpochTime t) 
                                                ageSeconds = cEpoch - pEpoch

                            where  
                                gzt cd = case cd of 
                                            Nothing   -> Left  $ False 
                                            Just   lt -> Right $ cvtToUtc lt
                                
                                cvtToUtc t = localTimeToUTC utc $ fst t
        
        cvtDate dt        = (strptime "%a, %Od %b %Y %OH:%OM:%OS %z" dt)
        waitAndRecur fd = do 
                            putStrLn "Waiting"
                            putMVar fm fd
                            threadDelay d
                            updateFeed h d fs fm
                
addMessage m = putMVar messages m

utcTimeToEpochTime :: UTCTime -> EpochTime
utcTimeToEpochTime = convert

run :: Net ()
run = do
    setNick     nick
    setUser     (nick++" 0 * :haskelldev bot")
    joinChannel ircChannel
    asks socket >>= listen 

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

setUser :: String -> Net ()
setUser u = write "USER" u

setNick :: String -> Net ()
setNick n = write "NICK" n

joinChannel :: String -> Net ()
joinChannel ch = write "JOIN" ch

io :: IO a -> Net a
io = liftIO

listen :: Handle -> Net ()
listen h = forever $ do
        s <- init `fmap` io (hGetLine h)
        io $ putStrLn s
        eval s

eval :: String -> Net ()
eval x =  
        if      ping                then pong
        else case cmdIs of
        "!quit"     -> quitCmd
        "!say"      -> sayCmd
        "!uptime"   -> uptimeCmd 
        _           -> ret
    where
        ping         = "PING :" `isPrefixOf` x
        pong         = write "PONG" (':' : drop 6 x)
        parseIrcCmd  = head (splitOn "!" x) : tail (splitOn " " x)
        clean        = drop 1 . dropWhile (/= ':') . drop 1
        ircUser      = drop 1 $ parseIrcCmd !! 0
        chan         = if ((parseIrcCmd !! 2) == nick) then ircUser else (parseIrcCmd !! 2) 
        botCmd       = clean x
        isBotCmd     = (length botCmd) > 0 && (botCmd !! 0) == '!'
        cmdIs        = if (not $ null (words botCmd)) then (words botCmd !! 0) else ""
        sayCmd       = if isAdmin then say else ret 
        quitCmd      = if isAdmin then quitIrc else ret
        uptimeCmd    = uptime >>= (privmsgTo chan)
        say          = privmsgTo chan $ drop 4 botCmd
        isAdmin      = ircUser `elem` admins 
        ret          = io $ return ()
                                        
uptime :: Net String
uptime = do
        now <- io getClockTime
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

writeFeedData h f = do
        writeItem `mapM` f
    where 
        writeItem x = addMessage $ ircChannel ++ " : " ++ name x ++ ": " ++ text x ++ " <" ++ url x ++ ">"
        name x      = (feedName (feedItemSource x))
        text x      = (feedItemTitle x)
        url  x      = take (39 + (length $ name x)) (feedItemUrl x) 

quitIrc :: Net ()
quitIrc = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)

privmsg :: String -> Net ()
privmsg text = privmsgTo ircChannel text

privmsgTo :: String -> String -> Net ()
privmsgTo to text = io $ addMessage (to ++ " :" ++ text)
