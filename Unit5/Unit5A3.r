## MITx Analytics Edge
## Michael Hunt

## Unit 5 Assignment Three

## SEPARATING SPAM FROM HAM (PART 1)

# Load the dataset


emails = read.csv("./data/emails.csv", stringsAsFactors=FALSE)
str(emails)

#1.1 How many emails?
nrow(emails)

#1.2 How many of these were spam?
sum(emails$spam)

#1.3 How many characters in longest email
emails$nchar<-nchar(emails$text)
max(emails$nchar)

#1.4 which row has the shortest email?
emails[nchar(emails$text)==min(nchar(emails$text)),]

## PREPARING THE CORPUS

#2.1 Create corpus

library(tm)
corpus = Corpus(VectorSource(emails$text))
# Pre-process data
corpus = tm_map(corpus, tolower)
# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
length(stopwords("english"))
#create a matrix
dtm = DocumentTermMatrix(corpus)
dtm
ncol(dtm)

#2.2 Remove sparse terms
spdtm= removeSparseTerms(dtm, 0.95)
ncol(spdtm)

# 2.3 Create data frame
emailsSparse = as.data.frame(as.matrix(spdtm))
names(emailsSparse)<-make.names(names(emailsSparse))

# which word occurs most frequently
library(dplyr)
sums<-as.numeric(colSums(emailsSparse))
names<-names(emailsSparse)
Absums<-data.frame(names,sums)
arrange(Absums,-sums)[1,]

# achieves the same in one line of code!
which.max(colSums(emailsSparse))

#2.4 
emailsSparse$spam=emails$spam
ham<-subset(emailsSparse,emailsSparse$spam==0)
which(colSums(ham)>=5000)

#2.5
spam<-subset(emailsSparse,emailsSparse$spam==1)
which(colSums(spam)>=1000)

## BUILDING MACHINE LEARNING MODELS

#3.1

#Create training and test sets

emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
library(rpart)
library(rpart.plot)

set.seed(123)
spl = sample.split(emailsSparse$spam, SplitRatio =0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

#Train Logistic regression model
spamLog<-glm(spam ~ . ,family=binomial,data=train)
summary(spamLog)

predLog<-predict(spamLog,train,type="response")
plot(predLog)
max(predLog)
