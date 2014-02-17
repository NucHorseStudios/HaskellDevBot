{-# LANGUAGE ScopedTypeVariables #-}
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

type Net = ReaderT Bot IO

data Bot =  Bot {
                socket    :: Handle,
                startTime :: ClockTime,
                messages  :: TVar [String]
            }

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
            loop st    = catch (runReaderT run st) (\(e :: IOException) -> return ()) 

connect :: TVar [String] -> IO Bot
connect mv
    = notify $ do
        h  <- connectTo server $ PortNumber $ fromIntegral port
        t  <- getClockTime

        hSetBuffering h NoBuffering
        forkIO $ dispatchMessages h 1000000 mv
        mapM (spawnFeedProc h) feeds
        return (Bot h t mv)
    
    where
        notify a = bracket_
            ((printf "Connecting to %s ... " server)  >> (hFlush stdout))
            (putStrLn "done.")
            a

        spawnFeedProc :: Handle -> FeedSource -> IO (ThreadId)
        spawnFeedProc h fs 
            = do
                fd <- atomically $ newTVar ([] :: [FeedData]) 

                forkIO $ updateFeed h rt fs fd mv
            
            where
                rt :: Int
                rt = (feedRefreshTime fs) * 1000000
    

run :: Net ()
run 
    = do
        setNick     nick
        setUser     (nick++" 0 * :haskelldev bot")
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

setUser :: String -> Net ()
setUser u = write "USER" u

setNick :: String -> Net ()
setNick n = write "NICK" n

joinChannel :: String -> Net ()
joinChannel ch = write "JOIN" ch

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

updateFeed :: Handle -> Int -> FeedSource -> TVar [FeedData] -> TVar [String] -> IO ()
updateFeed h d fs fd mv
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
                    else (writeFeedData h mv (getNewItems od d)) >> waitAndRecur

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
            waitAndRecur = threadDelay d >> updateFeed h d fs fd mv
                
ping :: String -> Bool
ping x = "PING :" `isPrefixOf` x

pong :: String -> Net ()
pong x = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval x =   
    if (ping x) then (pong x)
    else 
    case cmdIs of
        "!quit"     -> quitCmd
        "!say"      -> sayCmd
        "!uptime"   -> uptimeCmd 
        "!credits"  -> creditsCmd
        "!help"     -> helpCmd
        "!source"   -> sourceCmd
        _           -> ret
    where
        parseIrcCmd  = words x
        clean        = drop 1 . dropWhile (/= ':') . drop 1
        ircUser      = drop 1 (splitOn "!" (parseIrcCmd !! 0) !! 0)
        chan         = if ((parseIrcCmd !! 2) == nick) then ircUser else (parseIrcCmd !! 2) 
        botCmd       = clean x
        cmdIs        = if (not $ null (words botCmd)) then (words botCmd !! 0) else ""
        sayCmd       = if isAdmin then say else ret  
        quitCmd      = if isAdmin then quitIrc else ret
        uptimeCmd    = uptime >>= (privmsgTo chan)
        creditsCmd   = privmsgTo chan "Programmed by DrAwesomeClaws."
        helpCmd      = privmsgTo chan "Commands: !help !uptime !credits !source !say !quit !listfeeds"
        sourceCmd    = privmsgTo chan "https://github.com/NucHorseStudios/HaskellDevBot"
        say          = privmsgTo chan $ drop 4 botCmd
        isAdmin      = (ircUser `elem` admins)
        ret          = return ()
                                        
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

writeFeedData :: Handle -> TVar [String] -> [FeedData] -> IO [()]
writeFeedData h mv f 
    = do
        writeItem `mapM` f
    where 
        writeItem x = atomically $ addMessage mv (m x)
        m    x      = ircChannel ++ " : " ++ name x ++ ": " ++ text x ++ " <" ++ url x ++ ">"
        name x      = (feedName (feedItemSource x))
        text x      = (feedItemTitle x)
        url  x      = take (39 + (length $ name x)) (feedItemUrl x) 

quitIrc :: Net ()
quitIrc = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)

privmsg :: String -> Net ()
privmsg text = privmsgTo ircChannel text

privmsgTo :: String -> String -> Net ()
privmsgTo to text 
    = do 
        mv <- asks messages
        io $ atomically $ addMessage mv (to ++ " :" ++ text)

