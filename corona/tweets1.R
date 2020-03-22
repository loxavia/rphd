#Twitter Analysis - Corona
#https://www.worldometers.info/coronavirus/coronavirus-death-toll/

library(pacman)  #loading multiple libraries

pacman::p_load(ggplot2, dplyr, rvest, xml2, gridExtra, reshape2, tidyverse)
options(scipen = T)
pacman::p_load(curl, rtweet, ROAuth,syuzhet, httpuv, tidytext)

#Two methods of authentical - Browser and Access Token
twitter_token <- create_token(
  app = "DUrtweet",
  consumer_key = "0glwhLXXAahrusA3Dxvhou82M",
  consumer_secret = "bItULC8KINiNFIWcMBDekO6IztIoHeQ8yD6STqxbvIBxRCePZx")
#2nd Method: Access token/secret method
create_token(
  app = "DUrtweet",
  consumer_key = "0glwhLXXAahrusA3Dxvhou82M",
  consumer_secret = "bItULC8KINiNFIWcMBDekO6IztIoHeQ8yD6STqxbvIBxRCePZx",
  access_token = "14366551-KKd2B8pb5as86RKNf6sgULoM2aoL1JxspMzstXH8h",
  access_secret = "6s3lIn4YtppBLnj9H4xsHvtXWAHeywTfaqcwmfBWfWcO5")

## check to see if the token is loaded
identical(twitter_token, get_token())  #TRUE - Working

#search for tweets----
#q-word, n-no of tweets (max 18000)
tweetsJC <- search_tweets(q="JantaCurfew", n = 100, token = twitter_token)
tweetsJC
names(tweetsJC)
head(tweetsJC, n=3)
tweetsJC %>% select(created_at, text) %>% head(3) 
#rtweet::write_as_csv(tweetsJC, 'E:/data/tweetsJC.csv')  #save tweets in CSV

#exclude retweets
tweetsJCer <- search_tweets(q="JantaCurfew", n = 100, token = twitter_token, include_rts = F)

#who tweeted---
tweetsJCer %>% select(screen_name) %>% unique()

#locations of users
users <- search_users(q='JantaCurfew', n=100)
head(users)

#plot users with locations
length(unique(users$location))
users %>% ggplot(aes(location)) + geom_bar() + coord_flip() + labs(x='Count', y='Location', title='Twitter Users - Unique Location')

#top locations---
str(users$location)

users$location = as.character(users$location)
users %>% filter(location =='') %>% mutate(location ='NotFilled') %>% group_by(location) %>% tally()
str(users$location)
users[ users$location == '',] = 'NotKnown'
users %>% count(location, sort=T) %>% mutate(location = reorder(location, n)) %>% na.omit() %>% top_n(10) %>% ggplot(aes(x=location, y=n)) + geom_col() + coord_flip() + labs(x='Count', y='Location', title='Twitter Users - Unique Location') + scale_x_discrete()

# part 2----
#remove characters
twWOrt <- search_tweets(q="JantaCurfew", n = 100, token = twitter_token, include_rts = F)
twWOrt$stripped_text <- gsub('http.*', '', twWOrt$text)
twWOrt$stripped_text <- gsub('https.*', '', twWOrt$stripped_text)
head(twWOrt$stipped_text)

cleanTweets <- twWOrt %>% dplyr::select(stripped_text) %>% unnest_tokens(word, stripped_text)
head(cleanTweets)

cleanTweets %>% count(word, sort=T) %>% top_n(15) %>% mutate(word = reorder(word,n)) %>% ggplot(aes(x=word, y=n)) + geom_col() + xlab(NULL) + coord_flip() + labs(x='Count', y='Words', title='Count of Words')


#stopwords---
data(stop_words)
head(stop_words)

cleanTweets_words <- cleanTweets %>% anti_join(stop_words)
dim(cleanTweets_words)

cleanTweets_words %>% count(word, sort=T) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(x=word, y=n)) + geom_col() + coord_flip()


## plot time series of tweets
ts_plot(tweetsJC, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


identical(twitter_token, get_token())  #not working
#-----------------------------




## search for 18000 tweets using the rstats hashtag
rt <- search_tweets("#covid19", n = 60, include_rts = FALSE)
#----
rt

#-----
## plot time series of tweets
ts_plot(rt, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#-----
## search for 250,000 tweets containing the word data
rt <- search_tweets("data", n = 2500, retryonratelimit = TRUE)

#-----
## search for 10,000 tweets sent from the US
rt <- search_tweets("lang:en", geocode = lookup_coords("usa"), n = 10)

## create lat/lng variables using all available tweet and profile geo-location data
rt <- lat_lng(rt)
rt
## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))


#------
## random sample for 30 seconds (default)
rt <- stream_tweets("")

#------
## stream tweets from london for 60 seconds
rt <- stream_tweets(lookup_coords("london, uk"), timeout = 60)


#------
## stream london tweets for a week (60 secs x 60 mins * 24 hours *  7 days)
time =  10 #60 * 60 * 24 * 7
stream_tweets("narendramodi",timeout = time ,file_name = "narendramodi.json",  parse = FALSE)

## read in the data as a tidy tbl data frame
djt <- parse_stream("narendramodi.json")

#------



#-----
#trends
sf <- get_trends("san francisco")


#-----
## quick overview of rtweet functions
vignette("auth", package = "rtweet")

#----
## quick overview of rtweet functions
vignette("intro", package = "rtweet")

#------
## working with the stream
vignette("stream", package = "rtweet")


#-------
## working with the stream
vignette("FAQ", package = "rtweet")






neilfws <- get_timeline("neilfws", n = 100)
neilfws %>%
  glimpse()