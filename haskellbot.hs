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
import FeedTypes
import FeedParser

server      = "irc.freenode.org"
port        = 6667
ircChannel  = "##hb-testing"
nick        = "HaskellDevBot"
master      = "DrAwesomeClaws"
admins      = master : ["DrAwesomeClaws_"]
feeds       =   [
                    FeedSource { 
                        feedName = "r/gamedev", 
                        feedUrl  = "http://www.reddit.com/r/gamedev/new/.rss"
                    },
                    FeedSource {
                        feedName = "r/truegamedev",
                        feedUrl  = "http://www.reddit.com/r/truegamedev/new/.rss"
                    }
                ]
data Bot = Bot { 
            socket    :: Handle,
            startTime :: ClockTime 
           }

type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
  where
      disconnect = hClose . socket
      loop st    = catch (runReaderT run st) (\(e :: IOException) -> return ()) 

messages  = unsafePerformIO (newMVar [])
lastFeeds = unsafePerformIO (newMVar []) 

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
                            fm <- (newMVar [])
                            forkIO $ updateFeed h 50000000 fs fm 
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
    e  <- isEmptyMVar fm
    case e of
        True   -> do
                    putMVar fm fd
                    updateFeed h d fs fm
        False -> do
                    lfd <- takeMVar fm 
                    writeFeedData h (fd \\ lfd)
                    putStrLn $ "Updating Feed: " ++ (feedName fs)
                    putMVar fm fd
                    threadDelay d
                    updateFeed h d fs fm
                
                
addMessage m = putMVar messages m

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
         else if cmdIs "!quit"       then quitCmd
         else if cmdIs "!say"        then sayCmd
         else if cmdIs "!uptime"     then uptimeCmd 
         else    ret
    where
        ping         = "PING :" `isPrefixOf` x
        pong         = write "PONG" (':' : drop 6 x)
        parseIrcCmd  = head (splitOn "!" x) : tail (splitOn " " x)
        clean        = drop 1 . dropWhile (/= ':') . drop 1
        ircUser      = drop 1 $ parseIrcCmd !! 0
        ircChannel   = parseIrcCmd !! 2 
        botCmd       = clean x
        isBotCmd     = (length botCmd) > 0 && (botCmd !! 0) == '!'
        cmdIs t      = isBotCmd && t `isPrefixOf` botCmd
        sayCmd       = if isAdmin then say else ret 
        quitCmd      = if isAdmin then quitIrc else ret
        uptimeCmd    = (uptime >>= privmsg)
        say          = privmsgTo ircChannel $ drop 4 botCmd
        isAdmin      = ircUser `elem` admins 
        ret          = return ()
                                        
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
        writeItem x = addMessage $ ircChannel ++ " : [" ++ name x ++ "] " ++ text x ++ " <" ++ url x ++ ">"
        name x = (feedName (feedItemSource x))
        text x = (feedItemName x)
        url  x = (feedItemUrl x)

quitIrc :: Net ()
quitIrc = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)

privmsg :: String -> Net ()
privmsg text = privmsgTo ircChannel text

privmsgTo :: String -> String -> Net ()
privmsgTo to text = io $ addMessage (to ++ " :" ++ text)
