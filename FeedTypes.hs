module FeedTypes where

data FeedSource =
    FeedSource {
        feedName :: String,
        feedUrl  :: String
    }
    deriving (Eq, Show, Read)

data FeedData =
    FeedData {
        feedItemName   :: String,
        feedItemSource :: FeedSource,
        feedItemUrl    :: String
    }
    deriving (Eq, Show, Read)
