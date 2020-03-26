if (!require("rtweet")) install.packages("rtweet")
if (!require("tm")) install.packages("tm")
if (!require("SnowballC")) install.packages("SnowballC") 
if (!require("wordcloud")) install.packages("wordcloud") 
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("SentimentAnalysis")) install.packages("SentimentAnalysis")
if (!require("dplyr")) install.packages("dplyr")
if (!require('tidytext'))install.packages('tidytext')

library('rtweet')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("SentimentAnalysis")
library("dplyr")

token <- create_token(app = "",
                      consumer_key = "",
                      consumer_secret = "",
                      access_token = "",
                      access_secret = "",
                      set_renv = TRUE)

# get Tweets 

Tweets1 <- search_tweets("AI AND song", n =10000 ,retryonratelimit = TRUE, include_rts = FALSE ,lang = "en")

Tweets2 <- search_tweets('AI AND music', n =10000,retryonratelimit = TRUE, include_rts = FALSE ,lang = "en")

Tweets3 <- search_tweets('artificial intelligence AND music', n =10000,retryonratelimit = TRUE, include_rts = FALSE ,lang = "en")

Tweets4 <- search_tweets('artificial intelligence AND song', n =10000,retryonratelimit = TRUE, include_rts = FALSE ,lang = "en")

Tweets <- rbind(Tweets1, Tweets2,Tweets3,Tweets4)

help("search_tweets")
data.frame(Tweets)

write_as_csv(Tweets,"AI music Tweets.csv",prepend=TRUE,na="",fileEncoding = "UTF-8")

#------------------------------
#Part 1
#Wordcloud
setwd("CaraSave/HKBU/sem2/AI and digital media workshop/assignment/Group")

News <- read.csv('News sentiment.csv')

mt.v <- VectorSource(News$Content)
mt.c <- SimpleCorpus(mt.v)
inspect(mt.c)

mt.c.p <- tm_map(mt.c, content_transformer(tolower))
mt.c.p <- tm_map(mt.c.p, removeNumbers)
mt.c.p <- tm_map(mt.c.p, removeWords, stopwords("english"))
mt.c.p <- tm_map(mt.c.p, removePunctuation)
mt.c.p <- tm_map(mt.c.p, stripWhitespace)

inspect(mt.c.p)

dtm <- TermDocumentMatrix(mt.c.p)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

head(d, 50)


library(wordcloud2)
set.seed(1234)

wordcloud2(d ,size=2,color='random-light')

----------------------------------
#Part2 
#Sentiment analysis

time_sorted <- Tweets[order(Tweets$created_at),]
text_analysis <- analyzeSentiment(time_sorted$text)

write_as_csv(text_analysis,"#AI music value.csv",prepend=TRUE,na="",fileEncoding = "UTF-8")

directed <- convertToDirection(text_analysis)

write_as_csv(directed,"#AI music sentiment.csv",prepend=TRUE,na="",fileEncoding = "UTF-8")


plotSentiment(directed)

table(directed$SentimentGI)
mean(directed$WordCount)
