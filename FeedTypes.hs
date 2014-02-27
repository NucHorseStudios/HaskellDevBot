module FeedTypes where

data FeedSource =
    FeedSource {
        feedId :: Int,
        feedName :: String,
        feedRefreshTime :: Int,
        feedUrl  :: String
    }
    deriving (Eq, Show, Read)

data FeedData =
    FeedData {
        feedItemId      :: Integer,
        feedItemTitle   :: String,
        feedItemPubDate :: String,
        feedItemText    :: String,
        feedItemSource  :: FeedSource,
        feedItemUrl     :: String
    }
    deriving (Eq, Show, Read)
