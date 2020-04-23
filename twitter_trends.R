# Comparison of Twitter Trends
# Sean-Kelly Palicki 
# April 6, 2019


#
#
# Install Packages and load data
#
#
#

#install.packages('rtweet')
#install.packages('igraph')
#install.packages('tidytext')

library(rtweet)


api_key <- ""
api_secret <- ""
access_token <- ""
access_token_secret <- ""



create_token(
  app = "BCU_Data_Viz",
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_token_secret)

world <- get_trends("worldwide")

since <- '2019-03-31'
until <- '2019-04-07'

# Get Tweets 

jimin.tweets <- search_tweets("#jimin", n=10000, lang = "en", since = since, until = until, 
                              retryonratelimit = TRUE, type = 'mixed')

win.tweets <- search_tweets("#win", n=10000, lang = "en", since = since, until = until, 
                            retryonratelimit = TRUE, type = 'mixed')

bbc.tweets <- search_tweets("#bbcqt", n=10000, lang = "en", since = since, until = until, 
                            retryonratelimit = TRUE, type = 'mixed')

brexit.tweets <- search_tweets("#brexit", n=10000, lang = "en", since = since, until = until, 
                               retryonratelimit = TRUE, type = 'mixed')

bilboard.tweets <- search_tweets("#btsbillboardtopgroup", n=10000, lang = "en", since = since, until = until, 
                                 retryonratelimit = TRUE, type = 'mixed')

# Save data for visualization in Tableau
setwd("C:/Users/Sean/Desktop/S1 BCUBDA Textbooks/Social_Network/")
write_as_csv(bbc.tweets, 'bbc.csv')
write_as_csv(bilboard.tweets, 'bilboard.csv')
write_as_csv(jimin.tweets, 'jimin.csv')
write_as_csv(win.tweets, 'win.csv')
write_as_csv(brexit.tweets, 'brexit.csv')

bbc <- read.csv(file = 'bbc.csv')
bilboard <- read.csv(file = 'bilboard.csv')
jmin <- read.csv(file = 'jimin.csv')
win <- read.csv(file = 'win.csv')
brexit <- read.csv(file = 'brexit.csv')

library(dplyr)
library(tidyr)

master <- bind_rows(list(bbc, bilboard, brexit, jmin, win), .id="id")

write.csv(master, 'total_tweet_trending.csv')





