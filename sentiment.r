library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(sentiment)
library(httr)

prepareTweets <- function(rawTweets) {
  tweets = sapply(rawTweets, function(x) x$getText())
  tweets = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)
  tweets = gsub("@\\w+", "", tweets)
  tweets = gsub("[[:punct:]]", "", tweets)
  tweets = gsub("[[:digit:]]", "", tweets)
  tweets = gsub("http\\w+", "", tweets)
  tweets = gsub("[^a-zA-Z\ ]", "", tweets)
  tweets = gsub("[ \t]{2,}", "",tweets)
  tweets = gsub("^\\s+|\\s+$", "", tweets)
  
  tryToLower = function(x)
  {
    y = NA
    catch_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(catch_error, "error"))
      y = tolower(x)
    
    return(y)
  }
  
  tweets = sapply(tweets, tryToLower)
  tweets = tweets[!is.na(tweets)]
  names(tweets) = NULL
  
  return(tweets)
}

phrase <- "@drose"
count <- 1000

oauth_endpoints("twitter")
api_key <- "1pIyxQHrLF4wUdUmpCC1q6tPQ"
access_token <-  "27256931-yTQdbYSxpusswOJbPgSQzZYc00xEbxkqEehgDUitM"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweetsWithMeta = searchTwitter(phrase, n=count, lang="en")
tweets = prepareTweets(tweetsWithMeta)

emotionClassification = classify_emotion(tweets, algorithm="bayes", prior=1.0)
emotion = emotionClassification[,7]
emotion[is.na(emotion)] = "unknown"

polarityClassification = classify_polarity(tweets, algorithm="bayes")
polarity = polarityClassification[, 4]

sentiment = data.frame(text=tweets, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
sentiment = within(sentiment, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

emos <- levels(factor(sentiment$emotion))
emosLength <- length(emos)
emoDocs <- rep("", emosLength)

for (i in 1:emosLength)
{
  tmp = tweets[emotion == emos[i]]
  emoDocs[i] = paste(tmp, collapse=" ")
}

emoDocs = removeWords(emoDocs, stopwords("english"))

corpus = Corpus(VectorSource(emoDocs))
termDocumentMatrix = TermDocumentMatrix(corpus)
termDocumentMatrix = as.matrix(termDocumentMatrix)
colnames(termDocumentMatrix) = emos

dev.off()
ggplot(sentiment, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle(paste("Sentiment Analysis of Tweets on Twitter about", phrase)) +
  theme(legend.position="right") + 
  ylab("Number of Tweets") + 
  xlab("Emotion Categories")

ggplot(sentiment, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  ggtitle(paste("Sentiment Analysis of Tweets on Twitter about", phrase)) +
  theme(legend.position="right") + 
  ylab("Number of Tweets") + 
  xlab("Polarity Categories")

comparison.cloud(termDocumentMatrix, 
                 colors = brewer.pal(emosLength, "Dark2"), 
                 scale = c(3,.5), 
                 random.order = FALSE, 
                 title.size = 1.5)
