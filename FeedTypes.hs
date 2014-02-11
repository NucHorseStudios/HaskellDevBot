module FeedTypes where

data FeedSource =
    FeedSource {
        feedName :: String,
        feedRefreshTime :: Int,
        feedUrl  :: String
    }
    deriving (Eq, Show, Read)

data FeedData =
    FeedData {
        feedItemTitle   :: String,
        feedItemPubDate :: String,
        feedItemSource  :: FeedSource,
        feedItemUrl     :: String
    }
    deriving (Eq, Show, Read)
