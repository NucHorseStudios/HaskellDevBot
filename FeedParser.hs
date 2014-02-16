module FeedParser where

import FeedTypes
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Html.Generate(showattr)
import Data.Char
import Data.List

import Control.Monad.Reader     
import Control.Exception  
import Network.URI
import Network.HTTP
import Data.Maybe

downloadUrl url 
    =   catch makeRequest onError
        where 

            makeRequest :: IO (Either String String)
            makeRequest = do 
                    resp <- simpleHTTP request
                            
                    case resp of
                        Left  x -> return $ Left ("Error connectng: " ++ show x)
                        Right r -> handleResponse r
            
            handleResponse :: Response [Char] -> IO (Either String String)
            handleResponse r = 
                case rspCode r of
                    (2,_,_) -> return $ Right (rspBody r)
                    (3,_,_) -> handleRedirect r
                    _       -> return $ Left (show r)
            
            handleRedirect :: Response [Char] -> IO (Either String String)
            handleRedirect r = 
                case findHeader HdrLocation r of
                    Nothing -> return $ Left (show r)
                    Just url -> downloadUrl url
                                        
            onError :: IOException -> IO (Either String String)
            onError e  = return $ Left "Error Connecting."
            
            request = Request {
                    rqURI       = uri,
                    rqMethod    = GET,
                    rqHeaders   = [],
                    rqBody      = ""
                  }
            uri     = fromJust $ parseURI url

getFeedData :: FeedSource -> IO (Maybe [FeedData])
getFeedData fs 
    = do 
        resp <- downloadUrl (feedUrl fs) 
        case resp of 
            Left x      -> return Nothing
            Right doc   -> return $ Just $ ((itemToFeedData fs) `map` (items doc))
    where 
        onError e                   = return Nothing 
        items   doc                 = feedItems (parse doc name)
        name                        = (feedName fs) 

data FeedItem = 
     FeedItem {     
        itemTitle   :: String,
        itemPubDate :: String,
        itemUrl     :: String
     }
     deriving (Eq, Show, Read)

data Feed = 
     Feed { 
        channelTitle :: String,
        feedItems    :: [FeedItem]
    }
    deriving (Eq, Show, Read)

itemToFeedData :: FeedSource -> FeedItem -> FeedData
itemToFeedData fs item =
    FeedData {
        feedItemTitle   = itemTitle item,
        feedItemSource  = fs,
        feedItemPubDate = itemPubDate item,
        feedItemUrl     = itemUrl item  
    }

parse :: String -> String -> Feed
parse content name =
        Feed {
            channelTitle = getTitle doc,
            feedItems    = getEnclosures doc
        }
    where
        parseResult = xmlParse name (stripUnicodeBOM content)
        doc         = getContent parseResult

        getContent (Document _ _ e _) = CElem e noPos

        stripUnicodeBOM :: String -> String
        stripUnicodeBOM ('\xfeff':x) = x
        stripUnicodeBOM x = x

channel = tag "rss" /> tag "channel"

getTitle doc =
    contentToStringDefault "Untitled FeedSource"
        (channel /> tag "title" /> txt $ doc)

getEnclosures doc =
    map procFeedItem $ getFeedItems doc
    where
        getFeedItems      = channel /> tag "item" 
        procFeedItem item =
             FeedItem {
                itemTitle   = title,
                itemUrl     = link,
                itemPubDate = pubDate
            }
            where 
                title   = contentToStringDefault "Untitled FeedData"
                          (keep /> tag "title" /> txt $ item)
                link    = contentToString (keep /> tag "guid"  /> txt $ item)
                pubDate = contentToString (keep /> tag "pubDate" /> txt $ item)

contentToStringDefault msg [] = msg
contentToStringDefault _   x  = contentToString x

contentToString c =
    concatMap procContent c
    where
        procContent x = verbatim $ 
                            keep /> txt $ 
                            CElem (unesc (fakeElem x)) noPos
        fakeElem x    = Elem (N "fake") [] [x]
        unesc         = xmlUnEscape stdXmlEscaper

