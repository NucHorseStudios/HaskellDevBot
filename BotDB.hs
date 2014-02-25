module BotDB where

import Control.Monad (when)
import Database.HDBC
import Database.HDBC.Sqlite3 
import Data.List (sort)
import Data.List.Utils
import Data.Maybe
import BotDataTypes 
import BotConfig
import FeedTypes

connectDb :: FilePath -> IO Connection
connectDb fp 
    = do 
        dbh <- connectSqlite3 fp
        prepDB dbh
        return dbh

prepDB :: IConnection c => c -> IO ()
prepDB dbh
    = do 
        tables <- getTables dbh
        
        when (not ("users" `elem` tables)) $ do 
            run dbh "CREATE TABLE users ( \
                        \ user_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \ 
                        \ nick TEXT NOT NULL UNIQUE, \ 
                        \ last_seen INTEGER NOT NULL)" []

            run    dbh ("INSERT INTO users (nick, last_seen) values ('" ++ master ++ "', 0)") []

            return ()
        
        when (not ("access_groups" `elem` tables)) $ do
            run dbh "CREATE TABLE access_groups ( \
                        \ access_group_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \   
                        \ name TEXT NOT NULL UNIQUE)" []

            run    dbh "INSERT INTO access_groups (name) values ('admin')" []
            run    dbh "INSERT INTO access_groups (name) values ('friend')" []
            
            return ()

        when (not ("user_access_groups" `elem` tables)) $ do
            run dbh "CREATE TABLE user_access_groups ( \
                        \ user_access_group_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
                        \ user_id INTEGER NOT NULL, \
                        \ access_group_id INTEGER NOT NULL, \
                        \ UNIQUE (user_id, access_group_id) ON CONFLICT IGNORE)" []

            run    dbh "INSERT INTO user_access_groups (user_id, access_group_id) VALUES (1, 1)" []

            return ()

        when (not ("feedsources" `elem` tables)) $ do
            run dbh "CREATE TABLE feedsources ( \
                        \ feed_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
                        \ name TEXT NOT NULL UNIQUE, \
                        \ refresh_time INTEGER NOT NULL, \
                        \ url TEXT NOT NULL)" []
            
            run dbh " INSERT INTO feedsources \
                        \ (name, refresh_time, url) \
                    \ VALUES ('r/gamedev', 30, 'http://www.reddit.com/r/gamedev/new/.rss')" []
    
            run dbh " INSERT INTO feedsources \
                        \ (name, refresh_time, url) \
                    \ VALUES ('r/truegamedev', 30, 'http://www.reddit.com/r/truegamedev/new/.rss')" []
            
            run dbh " INSERT INTO feedsources \
                        \ (name, refresh_time, url) \
                    \ VALUES ('r/gamedevbottesting', 10, 'http://www.reddit.com/r/gamedevbottesting/new/.rss')" []
            
            return ()

        when (not ("feed_data" `elem` tables)) $ do
            run dbh "CREATE TABLE feed_data ( \
                        \ data_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
                        \ title TEXT, \
                        \ pub_date TEXT, \
                        \ feedsource_id INTEGER NOT NULL, \
                        \ post_text TEXT, \
                        \ url TEXT UNIQUE, \
                        \ dispatched INTEGER DEFAULT 0, \
                        \ UNIQUE (title, pub_date, feedsource_id, url) ON CONFLICT IGNORE)" []
            return ()

        when (not ("feedpost_notifications" `elem` tables)) $ do
            run dbh " CREATE TABLE feedpost_notifications ( \
                        \ notification_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, \
                        \ feed_id INTEGER NOT NULL, \
                        \ user_id INTEGER NOT NULL, \
                        \ UNIQUE (feed_id, user_id) ON CONFLICT REPLACE)" []

            return ()

        commit dbh


addUser :: IConnection c => c -> String -> Int -> IO (Maybe User)
addUser dbh u ls
    = do 
        user <- getUser dbh u
        case user of 
            Nothing ->  handleSql errorHandler $ run dbh insertQuery [toSql u, toSql ls]
            _       ->  handleSql errorHandler $ run dbh updateQuery [toSql ls, toSql u]
        commit dbh 
        getUser dbh u
    where 
        errorHandler e  = (fail $ failMessage e)
        failMessage  e  = "Error adding user"  ++ ": " ++ show e 
        insertQuery     = "INSERT INTO users (nick, last_seen) VALUES (?, ?)"
        updateQuery     = "UPDATE users SET last_seen = ? WHERE nick = ?"


addUser_ :: IConnection c => c -> String -> Int -> IO ()
addUser_ dbh u ls
    = do 
        user <- addUser dbh u ls

        case user of 
            _ -> return ()

getUser :: IConnection c => c -> String -> IO (Maybe User)
getUser dbh un 
    = do 
        r <- quickQuery' dbh selectQuery [toSql un]

        case r of 
            [[uId, uNick, uLs]] -> return $ Just $ User { 
                                                userId   = (fromSql uId), 
                                                nick     = (fromSql uNick),
                                                lastSeen = (fromSql uLs) 
                                            }
            e                   -> return Nothing
    where 
        failMessage  e  = "Error adding user"  ++ ": " ++ show e
        selectQuery     = "SELECT user_id, nick, last_seen FROM users WHERE nick = ?" 

getAccessGroupUsers :: IConnection c => c -> String -> IO (Maybe [User])
getAccessGroupUsers dbh gn 
    = do 
        r <- quickQuery' dbh selectQuery [toSql gn]
        case r of 
            [] -> return $ Nothing
            _  -> return $ Just $ map convertToUser r
    where 
        selectQuery = "SELECT u.user_id, u.nick, u.last_seen \
                        \ FROM users u, access_groups ag, user_access_groups uag \
                        \ WHERE ag.name = ? \
                            \ AND uag.access_group_id = ag.access_group_id \
                            \ AND uag.user_id = u.user_id"

getGroupByName :: IConnection c => c -> String -> IO (Maybe AccessGroup)
getGroupByName dbh gn
    = do
        g <- quickQuery' dbh selectQuery [toSql gn] 
        
        case g of 
            []  -> return Nothing
            [x] -> return $ Just $ convertToGroup (head g)
    where 
        selectQuery = "SELECT access_group_id, name FROM access_groups WHERE name = ?"

addFeedData :: IConnection c => c -> [FeedData] -> IO ()
addFeedData dbh fd 
    = do
        mapM insertData fd
        commit dbh
    where
        insertData d = runQuery (feedItemTitle d) (feedItemPubDate d) (feedId (feedItemSource d)) (feedItemUrl d)
        insertQuery = "INSERT INTO feed_data (title, pub_date, feedsource_id, url) VALUES(?, ?, ?, ?)"
        runQuery t pd fsid u = run dbh insertQuery [toSql t, toSql pd, toSql fsid, toSql u]

getNewFeedData :: IConnection c => c -> FeedSource -> IO ([FeedData])
getNewFeedData dbh fs
    = do 
        nd  <- quickQuery' dbh selectQuery [toSql (feedId fs)] 
        return $ map (convertToFeedData fs) nd
    where
        selectQuery = "SELECT data_id, title, pub_date, url \
                        \ FROM feed_data WHERE dispatched=0 AND feedsource_id = ?"
        
setDataDispatched :: IConnection c => c -> [FeedData] -> IO ()
setDataDispatched dbh fd
    = do
        run dbh updateQuery []
        commit dbh
    where
        updateQuery = "UPDATE feed_data SET dispatched = 1 WHERE data_id IN (" ++ (join ", " ids) ++ ")"
        ids         = map feedId fd
        feedId d    = show (feedItemId d)
        
addUserToAccessGroup :: IConnection c => c -> User -> AccessGroup -> IO ()
addUserToAccessGroup dbh u g
    = do
        gu <- getAccessGroupUsers dbh (groupName g)
        
        case gu of 
            Nothing -> return ()
            Just uu -> case (filter (\usr -> (nick usr) == (nick u)) uu) of
                        []  ->  (runQuery (userId u) (accessGroupId g)) >> commit dbh 
                        _   ->  return ()
    where 
        insertQuery = "INSERT INTO user_access_groups (user_id, access_group_id) VALUES (?, ?)"
        runQuery uid gid =  run dbh insertQuery [toSql uid, toSql gid]

removeUserFromAccessGroup :: IConnection c => c -> User -> AccessGroup -> IO ()
removeUserFromAccessGroup dbh u g
    = do 
        runQuery (userId u) (accessGroupId g) >> commit dbh >> return ()
    where 
        removeQuery      = "DELETE FROM user_access_groups WHERE user_id = ? AND access_group_id = ?"
        runQuery uid gid = run dbh removeQuery [toSql uid, toSql gid]

startNotificationsFor :: IConnection c => c -> User -> FeedSource -> IO ()
startNotificationsFor dbh u fs 
    = do
        putStrLn "On"
        runQuery (feedId fs) (userId u) >> commit dbh

    where
        insertQuery = "INSERT INTO feedpost_notifications (feed_id, user_id) VALUES (?, ?)"
        runQuery fid uid = run dbh insertQuery [toSql fid, toSql uid]

stopNotificationsFor :: IConnection c => c -> User -> FeedSource -> IO ()
stopNotificationsFor dbh u fs 
    = do
        putStrLn "Stop"
        runQuery (feedId fs) (userId u) >> commit dbh

    where
        deleteQuery = "DELETE FROM feedpost_notifications WHERE feed_id = ? AND user_id = ?"
        runQuery fid uid = run dbh deleteQuery [toSql fid, toSql uid]
        
notifiedUsers :: IConnection c => c -> FeedSource -> IO ([User])
notifiedUsers dbh fs
    = do
        r <- quickQuery' dbh selectQuery [toSql (feedId fs)]
        
        case r of 
            []      -> return ([] :: [User])
            x       -> return $ map convertToUser x 
    where
        selectQuery = "SELECT u.user_id, u.nick, u.last_seen \
                        \ FROM users u, feedpost_notifications fpn \
                        \ WHERE fpn.user_id = u.user_id AND fpn.feed_id = ?"

getFeeds :: IConnection c => c -> IO ([FeedSource])
getFeeds dbh
    = do
        r <- quickQuery' dbh selectQuery []
        
        case r of 
            []  -> return $ ([] :: [FeedSource])
            x   -> return $ map convertToFeedSource x 
    where 
        selectQuery = "SELECT feed_id, name, refresh_time, url FROM feedsources"

getFeed :: IConnection c => c -> Int -> IO (Maybe FeedSource)
getFeed dbh id 
    = do
        r <- quickQuery' dbh selectQuery [(toSql id)]
        
        case r of
            []     -> return $ Nothing
            (x:xs) -> return $ Just $ convertToFeedSource x
    where
        selectQuery =   " SELECT feed_id, name, refresh_time, url \
                        \ FROM feedsources WHERE feed_id = ?"

convertToFeedSource [fId, fn, rt, u]
    =   FeedSource {
            feedId = fromSql fId,
            feedName = fromSql fn,
            feedRefreshTime = fromSql rt,
            feedUrl = fromSql u
        }

convertToFeedData fs [fdId, fdTitle, fdPd, fdUrl]
    =   FeedData {
            feedItemId      = fromSql fdId,
            feedItemTitle   = fromSql fdTitle,
            feedItemPubDate = fromSql fdPd,
            feedItemSource  = fs,
            feedItemUrl     = fromSql fdUrl
        }

convertToGroup [gId, gName]
    =   AccessGroup {
            accessGroupId = fromSql gId,
            groupName = fromSql gName
        }

convertToUser [uId, uNick, uLs]
    =   User { 
            userId   = fromSql uId,
            nick     = fromSql uNick,
            lastSeen = fromSql uLs
        }
