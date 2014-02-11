module FeedParser where

import FeedTypes
import Text.XML.HaXml
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Html.Generate(showattr)
import Data.Char
import Data.List

import Control.Monad.Reader     
import Network.URI
import Network.HTTP
import Data.Maybe

downloadUrl url =
    do resp <- simpleHTTP request
       case resp of
            Left  x -> return $ Left ("Error connectng: " ++ show x)
            Right r ->
                  case rspCode r of
                    (2,_,_) -> return $ Right (rspBody r)
                    (3,_,_) -> 
                        case findHeader HdrLocation r of
                            Nothing -> return $ Left (show r)
                            Just url -> downloadUrl url
                    
                    _      -> return $ Left (show r)
    where
        request = Request {
                    rqURI       = uri,
                    rqMethod    = GET,
                    rqHeaders   = [],
                    rqBody      = ""
                  }
        uri     = fromJust $ parseURI url

getFeedData :: FeedSource -> IO [FeedData]
getFeedData fs =
    do resp <- downloadUrl (feedUrl fs)
       case resp of 
            Left x -> return []
            Right doc -> return $ (itemToFeedData fs) `map` (items doc)
       where 
            items doc = feedItems (parse doc name)
            name  = (feedName fs) 

data FeedItem = 
     FeedItem {     itemTitle :: String,
                    itemUrl   :: String
     }
     deriving (Eq, Show, Read)

data Feed = 
     Feed { 
            channelTitle :: String,
            feedItems :: [FeedItem]
    }
    deriving (Eq, Show, Read)

itemToFeedData :: FeedSource -> FeedItem -> FeedData
itemToFeedData fs item =
    FeedData {
        feedItemName    = itemTitle item,
        feedItemSource  = fs,
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
        procFeedItem item =
             FeedItem {
                itemTitle = title,
                itemUrl   = link
            }
            where 
                title = contentToStringDefault "Untitled FeedData"
                        (keep /> tag "title" /> txt $ item)
                
                link  = contentToString (keep /> tag "link"  /> txt $ item)

        getFeedItems      = channel /> tag "item"

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

