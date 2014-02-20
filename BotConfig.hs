module BotConfig where

import FeedTypes

server      = "irc.freenode.org"
port        = 6667
ircChannel  = "##hb-testing"
botNick     = "GamedevBotT"
master      = "DrAwesomeClaws"
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

