library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(sentiment)
library(httr)

#auth
oauth_endpoints("twitter")
api_key <- "1pIyxQHrLF4wUdUmpCC1q6tPQ"
access_token <-  "27256931-yTQdbYSxpusswOJbPgSQzZYc00xEbxkqEehgDUitM"


setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
