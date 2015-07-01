## MITx Analytics Edge
## Michael Hunt

## Unit 5 Assignment One

## DETECTING VANDALISM ON WIKIPEDIA

# Load the dataset

wiki = read.csv("./data/wiki.csv", stringsAsFactors=FALSE)
as.factor(wiki$Vandal)
str(wiki)

## BAGS OF WORDS

# 1.1
sum(wiki$Vandal)

# 1.2
#1) Create the corpus for the Added column, and call it "corpusAdded".
#2) Remove the English-language stopwords.
#3) Stem the words.
#4) Build the DocumentTermMatrix, and call it dtmAdded.

library(tm)
# Create corpus
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]
# Pre-process data (laready in lower case and with punc removed)
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
# Create matrix
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

# 1.3 
# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions,
# and call the new matrix sparseAdded.
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# 1.4 convert to data frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded)) # prepend all words with "A"

# Repeat 1.2 to 1.4 to create wordsRemoved data frame

library(tm)
# Create corpus
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved[[1]]
# Pre-process data (laready in lower case and with punc removed)
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
# Create matrix
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

# 1.3 
# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions,
# and call the new matrix sparseAdded.
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

# 1.4 convert to data frame
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved)) # prepend all words with "R"
# str(wordsRemoved)

# 1.5 combine the added and removed data frames
wikiWords = cbind(wordsAdded, wordsRemoved)

# Add in the outcome variable
wikiWords$Vandal = wiki$Vandal

# Split the data

library(caTools)

set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

# baseline accuracy on test set
table(test$Vandal)
table(test$Vandal)[1]/sum(table(test$Vandal))

# 1.6 Build a CART model and find accuracy on test set
# Build a CART model

library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal~., data=train, method="class")

# Make predictions on the test set
pred = predict(wikiCART, newdata=test)
pred[1:10,]
pred.prob = pred[,2]

# Compute accuracy on test set
ct<-table(test$Vandal,pred.prob >= 0.5)
ct
sum(diag(ct))/sum(ct)

# 1.7 plot the CART tree
prp(wikiCART)

# Although it beats the baseline, bag of words is not very predictive for this problem. 

## PROBLEM-SPECIFIC KNOWLEDGE

# 2.1 try another approach - key class of words eg URL
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP)

# 2.2 - new CART model
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wikiCART2 = rpart(Vandal~., data=wikiTrain2, method="class")
pred2 = predict(wikiCART2, newdata=wikiTest2)
pred2[1:10,]
pred2.prob = pred2[,2]
# Compute accuracy on test set - slight improvement
ct2<-table(wikiTest2$Vandal,pred2.prob >= 0.5)
ct2
sum(diag(ct2))/sum(ct2)

# 2.3 - what about number of words added or removed?
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

# 2.4 new CART model
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wikiCART2 = rpart(Vandal~., data=wikiTrain2, method="class")
# Make predictions on the test set
pred2 = predict(wikiCART2, newdata=wikiTest2)
pred2[1:10,]
pred2.prob = pred2[,2]

# Compute accuracy on test set - vastly improved
ct2<-table(wikiTest2$Vandal,pred2.prob >= 0.5)
ct2
sum(diag(ct2))/sum(ct2)

## USING NON-TEXTUAL DATA
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

# new CART model
wikiTrain2 = subset(wikiWords3, spl==TRUE)
wikiTest2 = subset(wikiWords3, spl==FALSE)
wikiCART2 = rpart(Vandal~., data=wikiTrain2, method="class")
# Make predictions on the test set
pred2 = predict(wikiCART2, newdata=wikiTest2)
pred2[1:10,]
pred2.prob = pred2[,2]
# Compute accuracy on test set - vastly improved again
ct2<-table(wikiTest2$Vandal,pred2.prob >= 0.5)
ct2
sum(diag(ct2))/sum(ct2)

# 3.2
prp(wikiCART2)
# By adding new independent variables, we were able to significantly improve our accuracy
# without making the model more complicated!
