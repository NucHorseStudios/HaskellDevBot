module BotDB where

import Control.Monad (when)
import Database.HDBC
import Database.HDBC.Sqlite3 
import Data.List (sort)
import BotDataTypes 
import BotConfig

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
            run dbh "CREATE TABLE users (\
                        \ user_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\ 
                        \ nick TEXT NOT NULL UNIQUE,\ 
                        \ last_seen INTEGER NOT NULL)" []

            run    dbh ("INSERT INTO users (nick, last_seen) values ('DrAwesomeClaws', 0)") []

            return ()
        
        when (not ("access_groups" `elem` tables)) $ do
            run dbh "CREATE TABLE access_groups (\
                        \ access_group_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\  
                        \ name TEXT NOT NULL UNIQUE)" []

            run    dbh "INSERT INTO access_groups (name) values ('admin')" []
            run    dbh "INSERT INTO access_groups (name) values ('friend')" []
            
            return ()

        when (not ("user_access_groups" `elem` tables)) $ do
            run dbh "CREATE TABLE user_access_groups (\
                        \ user_access_group_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                        \ user_id INTEGER NOT NULL,\
                        \ access_group_id INTEGER NOT NULL)" []

            run    dbh "INSERT INTO user_access_groups (user_id, access_group_id) VALUES (1, 1)" []

            return ()
        
        commit dbh

addUser :: IConnection c => c -> String -> Integer -> IO (Maybe User)
addUser dbh u ls
    = do 
        user <- getUser dbh u
        case user of 
            Nothing ->  handleSql errorHandler $ (run dbh insertQuery [toSql u, toSql ls]) >>
                        commit dbh >>
                        getUser dbh u
            _       ->  getUser dbh u
    where 
        errorHandler e  = (fail $ failMessage e)
        failMessage  e  = "Error adding user"  ++ ": " ++ show e 
        insertQuery     = "INSERT INTO users (nick, last_seen) VALUES (?, ?)"


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

convertToGroup [gId, gName]
    = AccessGroup {
        accessGroupId = fromSql gId,
        groupName = fromSql gName
    }

convertToUser [uId, uNick, uLs]
    = User { 
        userId   = fromSql uId,
        nick     = fromSql uNick,
        lastSeen = fromSql uLs
    }
