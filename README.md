# MGB-293-Sentiment-Analysis
# Applying Twitter authentication to run a real time sentiment analysis using R Studio.

#install packages

install.packages("twitteR")
install.packages("ROAuth")
install.packages("RCurl")
install.packages("stringr")
install.packages("tm"")
install.packages("ggmap")
install.packages("dplyr")
install.packages("plyr")
install.packages("tm")
install.packages("wordcloud")
install.packages("httpuv")


rm(list=ls(all=TRUE))


#load library
library(httpuv)
library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)
library(httr)

setwd("C:/Users/eesy2_000/Documents/MBA/Marketing A")

# Twitter Authe

key <- "fpM3O16SMPkl9VBho8WyIkhq3"
secret <- "TO9EUsU1R7u5mpoM8GUZrhWkgsqHe9zHXE79EPjdQKXwYa0gEh"

oauth_endpoints("twitter")

setup_twitter_oauth(key, secret) #type yes, hit enter,  type 0, hit enter, and wait to log into twitter in browser after running


# Data request paramaters

N=100  # tweets to request from each query
S=5000  # radius in miles


tweetvalue <- c("Donald") #Words in tweet to search for c("Donald", "Trump")

lats=c(38.9,40.7,37.7749,34.05,40.71) #city lats
lons=c(-77,-74,122.4194,118.24,74.00) #city lons

# Data request. Data restructure

donald=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter(tweetvalue,
              lang="en",n=N,resultType="recent",
                 geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

donaldtext=sapply(donald, function(x) x$getText())
donaldtext=unlist(donaldtext)

data=as.data.frame(cbind(tweet=donaldtext))

#Clean data

tweet=data$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", " ", tweet) #remove url
tweet <- gsub("@.*?\\s", " ", tweet) #remove @
tweet = gsub('[[:punct:]]', ' ', tweet) #remove punct
tweet = gsub('[[:cntrl:]]', ' ', tweet) #remove control characters
tweet = gsub("amp", ' ', tweet) #remove control characters
tweet = gsub("  "," ",tweet) #Change dounle spaces to single spaces
tweet=unlist(tweet)
data$tweet=tweet

# Word map

data$tweet <- str_replace_all(data$tweet, "Join","") #Change data somehow

corpus=Corpus(VectorSource(data$tweet)) # Create corpus

corpus=tm_map(corpus,tolower) # Convert to lower-case

corpus=tm_map(corpus,PlainTextDocument) # convert corpus to a Plain Text Document
               
corpus=tm_map(corpus,function(x) removeWords(x,stopwords())) # Remove stopwords            

col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
random.color=T, max.word=45, random.order=F,colors=col)

# Sentiment score

pos = scan('positivewords.txt', what='character', comment.char=';') #Get lists of positive and negative words
 
neg = scan('negativewords.txt', what='character', comment.char=';')

#Create function to identify positive or negative words

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
 
{
  
  require(plyr)
  
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  
  # or a vector as an "l" for us
  
  # we want a simple array ("a") of scores back, so we use
  
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
  
  # clean up sentences with R's regex-driven global substitute, gsub():
  
  sentence = gsub('[[:punct:]]', '', sentence)
  
  sentence = gsub('[[:cntrl:]]', '', sentence)

  sentence = gsub('d+', '', sentence)

  sentence <- str_replace_all(sentence, "Join","")

  # and convert to lower case:
  
  sentence = tolower(sentence)
  
  # split into words. str_split is in the stringr package
  
  word.list = str_split(sentence, '\\s+')
  
  # sometimes a list() is one level of hierarchy too much
  
  words = unlist(word.list)
  
  # compare our words to the dictionaries of positive & negative terms
  
  pos.matches = match(words, pos.words)
  
  neg.matches = match(words, neg.words)
  
  # match() returns the position of the matched term or NA
  
  # we just want a TRUE/FALSE:
  
  pos.matches = !is.na(pos.matches)
  
  neg.matches = !is.na(neg.matches)
  
  # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
  
  score = sum(pos.matches) - sum(neg.matches)
  
  return(score)
  
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
  
}



#Use function on data
analysis = score.sentiment(data$tweet, pos, neg)

#Remove 0 scored tweets
analysis = analysis[analysis$score != "0", ]

mean <- mean(analysis$score) #Calculate Mean
row <- nrow(analysis)

#Create histogram 

title <- paste("Sentiment of", row, "tweets containing", "(", tweetvalue, ")")
hist(analysis$score,breaks=seq(-6.5,6.5,by=1),xlab=c("Average Sentiment",round(mean, digits = 3)),main=title,
     border="black",col="skyblue", ylab="frequency")

abline(v = mean, col = "blue", lwd = 2)




                 
