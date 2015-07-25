## THE ANALYTICS EDGE 15.071x

## UNIT 7 A3

## Michael Hunt

## July 2015

## VISUALISING TEXT DATA USING WORD CLOUDS

# We will visualize the text of tweets about Apple.

# The csv file contains two variables:

# Tweet -- the text of the tweet
# Avg --   the sentiment of the tweet, as assigned by users of Amazon Mechanical
#          Turk. The score ranges on a scale from -2 to 2, where 2 means highly positive
#          sentiment, -2 means highly negative sentiment, and 0 means neutral sentiment.

# download the data
tweets=read.csv("./data/tweets.csv",stringsAsFactors=FALSE)

## PREPARING THE DATA

# pre-processing of the data
# note we do not do any stemming here - this would make the wordcloud hard to follow
library(tm)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))


#create a document term matrix
dtm = DocumentTermMatrix(corpus)

#  Create data frame
allTweets = as.data.frame(as.matrix(dtm))
names(allTweets)<-make.names(names(allTweets))

# allTweets has 1181 observations (rows, 1 per tweet)
# and 3780 variables (columns, 1 per unique word)


# which word occurs most frequently - can also see this by building the wordcloud,
# see below.
library(dplyr)
sums<-as.numeric(colSums(allTweets))
names<-names(allTweets)
Absums<-data.frame(names,sums)
arrange(Absums,-sums)[1:10,]

# number of unique words
nrow(Absums)

## 2. BUILDING A WORD CLOUD

# load wordcloud package
library(wordcloud)

#build a worcloud
wordcloud(names,sums,scale=c(3, 0.375))
# compare with
arrange(Absums,-sums)[1:10,]

# We see that one word ("apple") occurs much more  frequently than any other.
# Let's remove it and build a new word cloud.

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))

#create a document term matrix
dtm = DocumentTermMatrix(corpus)

#  Create data frame
allTweets = as.data.frame(as.matrix(dtm))
names(allTweets)<-make.names(names(allTweets))

library(dplyr)
sums<-as.numeric(colSums(allTweets))
names<-names(allTweets)
Absums<-data.frame(names,sums)
arrange(Absums,-sums)[1:10,]

# load wordcloud package
library(wordcloud)

#build the new wordcloud, now excluding "apple"
wordcloud(names,sums,scale=c(2, 0.25))
# compare with
arrange(Absums,-sums)[1:10,]

# both methods readily show that "iphone" is now the most common word.

## 3. SIZE AND COLOR
?wordcloud

# 3.1 - do word cloud only on negative tweets (Avg <= -1)
# see below for MUCH easier way to achieve this!
negtweets=subset(tweets,Avg<=-1)
ncorpus = Corpus(VectorSource(negtweets$Tweet))
ncorpus = tm_map(ncorpus, tolower)
ncorpus = tm_map(ncorpus, PlainTextDocument)
ncorpus = tm_map(ncorpus, removePunctuation)
ncorpus = tm_map(ncorpus, removeWords, c("apple",stopwords("english")))

#create a document term matrix
negdtm = DocumentTermMatrix(ncorpus)

#  Create data frame
negTweets = as.data.frame(as.matrix(negdtm))
names(negTweets)<-make.names(names(negTweets))

library(dplyr)
negsums<-as.numeric(colSums(negTweets))
negnames<-names(negTweets)
NegAbsums<-data.frame(negnames,negsums)
arrange(NegAbsums,-negsums)[1:10,]

# load wordcloud package
library(wordcloud)

#build the new wordcloud, now excluding "apple"
wordcloud(negnames,negsums,scale=c(2, 0.25))
# compare with
arrange(NegAbsums,-negsums)[1:10,]

# now for the easy way of doing it:
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets))

# 3.2
wordcloud(names,sums,scale=c(2, 0.25))

## 4. SELECTING A COLOR PALETTE

# install.packages("RColorBrewer")
library(RColorBrewer)

?brewer.pal()
display.brewer.all()

wordcloud(names,sums,scale=c(2, 0.25),min.freq=4,colors=brewer.pal(9, "Blues")[c(5:9)])
wordcloud(names,sums,scale=c(2, 0.25),min.freq=4,colors=brewer.pal(9, "YlOrRd")[c(5:9)])
