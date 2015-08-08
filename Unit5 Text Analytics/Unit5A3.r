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

# Train each of the models

#Train Logistic regression model
spamLog<-glm(spam ~ . ,family=binomial,data=train)
summary(spamLog)
predTrainLog<-predict(spamLog,train,type="response")

#plot(predLog)
predTrainLog<-data.frame(predTrainLog)

table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)

# Train CART model
spamCART = rpart(spam~., data=train, method="class")
prp(spamCART)

# Train RF model
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data = train)

##Get training set accuracies and AUC for each model

#Logistic model
#3.4 Training set accuracy of SpamLog, (@thr = 0.5)
ct<-table(train$spam,predTrainLog >= 0.5)
ct
sum(diag(ct))/sum(ct)
#3.5 Training set AUC of SpamLog
library(ROCR)
ROCRLogtrain = prediction(predTrainLog, train$spam)
as.numeric(performance(ROCRLogtrain, "auc")@y.values)

#CART model
#3.6 Training set accuracy of spamCART
predCART.prob<-predict(spamCART,train)[,2]
ct<-table(train$spam,predCART.prob >= 0.5)
ct
sum(diag(ct))/sum(ct)
#3.7 Training set AUC of SpamCART
library(ROCR)
ROCRCARTtrain = prediction(predCART.prob, train$spam)
as.numeric(performance(ROCRCARTtrain, "auc")@y.values)

#Random Forest model
# 3.8  Training set accuracy of spamRF, using a threshold of 0.5 for predictions? 
PredictForest.prob = predict(spamRF,train,type="prob")[,2]
ctrf<-table(train$spam, PredictForest.prob>0.5)
ctrf
sum(diag(ctrf))/sum(ctrf)
#3.9 Training set AUC of SpamRF
library(ROCR)
ROCRRFtrain = prediction(PredictForest.prob, train$spam)
auc = as.numeric(performance(ROCRRFtrain, "auc")@y.values)
auc

## EVALUATING ON THE TEST SET  

#Logistic model
PredictLogTest <- predict(spamLog, newdata=test, type="response")
ctrf<-table(test$spam, PredictLogTest>0.5)
ctrf
#accuracy of SpamLog on test set
sum(diag(ctrf))/sum(ctrf)
#AUC of SpamLog on test set
ROCRLogtest = prediction(PredictLogTest, test$spam)
as.numeric(performance(ROCRLogtest, "auc")@y.values)

#CART model
predCARTtest.prob = predict(spamCART, newdata=test)[,2]
#accuracy of SpamCART on test set
ct<-table(test$spam,predCARTtest.prob >= 0.5)
ct
sum(diag(ct))/sum(ct)
#Test set AUC of SpamCART
ROCRCARTtest = prediction(predCARTtest.prob, test$spam)
as.numeric(performance(ROCRCARTtest, "auc")@y.values)

#RF model
PredictForest.Test.prob = predict(spamRF, newdata=test, type="prob")[,2]
ctrf<-table(test$spam, PredictForest.Test.prob>0.5)
ctrf
#accuracy of SpamRF on test set
sum(diag(ctrf))/sum(ctrf)
#AUC of SpamRF on test set
ROCRRFtest = prediction(PredictForest.Test.prob, test$spam)
as.numeric(performance(ROCRRFtest, "auc")@y.values)

## The Logistic model does best in accuracy and AUC on the training set, but worst
## on the test set, where the RF model does best. The Logistic model is hugely
## overfitted.
