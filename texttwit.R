
#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library("twitteR")
#install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='BagGgBbanzbdpPNNp8Uy6TQBP', # Consumer Key (API Key)
                         consumerSecret='pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',                
                         accessURL='https://api.twitter.com/oauth/access_token',                
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Access Token Secret

setup_twitter_oauth("BagGgBbanzbdpPNNp8Uy6TQBP", # Consumer Key (API Key)
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", #Consumer Secret (API Secret)
                    "1076425245521731584-Ev31ZLB7Cf0idVMqDI8BxiVG2SgRnu",  # Access Token
                    "ZVUw0Z0mFrX7d6sjQxuB08l48JHhmnjmlAm86G2OPG7BS")  #Access Token Secret

#registerTwitterOAuth(cred)
origop <- options("httr_oauth_cache")
options(httr_oauth_cache = TRUE)

Tweets <- userTimeline('climate', n = 1000,includeRts = T)

TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)

View(TweetsDF)
setwd('H://RStudio')
write.csv(TweetsDF, "Tweets_Climate.csv",row.names = F)

getwd()

# 
handleTweets <- searchTwitter('cyclone', n = 10000)
# handleTweetsDF <- twListToDF(handleTweets)
# dim(handleTweetsDF)
# View(handleTweetsDF)
# #handleTweetsMessages <- unique(handleTweetsDF$text)
# #handleTweetsMessages <- as.data.frame(handleTweetsMessages)
# #write.csv(handleTweetsDF, "TefalHandleTweets.csv")
# 
library(rtweet)
climate <-read.csv(file.choose())
head(climate$text)
?Corpus
library(tm)
clim<-Corpus(VectorSource(climate))
inspect(clim[1:5])
climate$stripped_text
clim$stripped_text <-gsub("http.*","",climate$text)
clim$stripped_text <-gsub("http.*","",climate$stripped_text)
install.packages("tidytext")
library(tidytext)
install.packages(c("mnormt", "psych", "SnowballC", "hunspell", 
                   "broom", "tokenizers", "janeaustenr"))
library(dplyr)
library(ggplot2)
climate_tweets_clean <- climate%>%dplyr::select(text)%>%unnest_tokens(word,text)
climate_tweets_clean %>% 
  count(word,sort=TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in #YouthSDGs tweets")
data("stop_words")
head(stop_words)
climate_tweets_words <- climate_tweets_clean %>%anti_join(stop_words)
climate_tweets_words %>%
  count(word,sort=TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in Climate tweets with stop words")
  
nrow(climate_tweets_clean)
library(wordcloud) 
library(reshape2)
climate_tweets_words%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment,sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("blue","purple"),
                   max.words = 150)
  