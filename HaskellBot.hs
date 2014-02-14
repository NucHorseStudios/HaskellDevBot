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

data Bot = Bot { 
            socket    :: Handle,
            startTime :: ClockTime, 
            lastPong  :: PongTime
           }

type Net = ReaderT Bot IO
type PongTime = TVar UTCTime

main :: IO ()
main =  do 
        ct <- getCurrentTime
        pt <- atomically $ newTVar ct 

        atomically $ setPongTime pt ct         
        --forkIO $ botWatchDog pt   
        forever $ catch (startBot pt) (\(e :: IOException) -> threadDelay 5 >> return ()) 

startBot :: PongTime -> IO ()
startBot pt =   bracket (connect pt) disconnect loop 
                where
                    disconnect = hClose . socket
                    loop st    = catch (runReaderT run st) (\(e :: IOException) -> return ()) 

messages     = unsafePerformIO (newMVar [])
addMessage m = putMVar messages m

connect :: PongTime -> IO Bot
connect pt = notify $ do
        h  <- connectTo server $ PortNumber $ fromIntegral port
        t  <- getClockTime

        hSetBuffering h NoBuffering
        forkIO $ (dispatchMessages h 1000000)
        mapM (spawnFeedProc h) feeds
        return (Bot h t pt)
    where
        spawnFeedProc h fs = do
                                fm <- newEmptyMVar
                                forkIO $ updateFeed h ((feedRefreshTime fs) * 1000000) fs fm 
        notify a = bracket_
            (printf "Connecting to %s ... " server >> hFlush stdout)
            (putStrLn "done.")
            a

getLastPong :: PongTime -> STM UTCTime
getLastPong pt = readTVar pt

botWatchDog :: PongTime -> IO ()
botWatchDog pt = do
                    ct <- getCurrentTime
                    lp <- atomically $ getLastPong pt

                    if (isWithin 600 ct lp) 
                    then (threadDelay  6000000) >> botWatchDog pt
                    else startBot pt

dispatchMessages :: Handle -> Int -> IO ()
dispatchMessages h d =  do  m <- takeMVar messages  
                            
                            hPutStrLn h $ "PRIVMSG " ++ m 
                            putStrLn $ "PRIVMSG " ++ m 
                            threadDelay d 
                            dispatchMessages h d

isWithin :: Int -> UTCTime -> UTCTime -> Bool
isWithin i a b = (fromEnum $ (utcTimeToEpochTime a) - (utcTimeToEpochTime b)) < i

utcTimeToEpochTime :: UTCTime -> EpochTime
utcTimeToEpochTime = convert

updateFeed :: Handle -> Int -> FeedSource -> MVar [FeedData] -> IO ()
updateFeed h d fs fm = do
    nd   <- getFeedData fs
    lfd  <- tryTakeMVar fm 

    putStrLn $ "Updating Feed: " ++ (feedName fs)

    case lfd of
        Nothing  -> waitAndRecur nd
        Just od  -> writeFeedData h (getNewItems od nd) >> waitAndRecur nd
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
       
        waitAndRecur :: [FeedData] -> IO ()
        waitAndRecur nd = putMVar fm nd >> threadDelay d >> updateFeed h d fs fm
                
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
listen h = do
            s  <- io $ timeout (3 * 1000000) $ init `fmap` (hGetLine h)
            case s of 
                Nothing -> (io $ putStrLn "Timeout") >> (io $ hPutStrLn h " ") >> listen h
                Just s  -> (eval s) >> (io $ putStrLn s) >> listen h

ping :: String -> Bool
ping x = "PING :" `isPrefixOf` x

setPongTime :: PongTime -> UTCTime -> STM ()
setPongTime pt t = writeTVar pt t

pong :: String -> Net ()
pong x = do
         ct <- io $ getCurrentTime
         pt <- asks lastPong
         io $ atomically $ setPongTime pt ct
         write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval x = do
            if (ping x) then (pong x)
            else case cmdIs of
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
uptime = do
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

writeFeedData :: Handle -> [FeedData] -> IO [()]
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
