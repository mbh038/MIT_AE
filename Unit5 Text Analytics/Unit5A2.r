## MITx Analytics Edge
## Michael Hunt

## Unit 5 Assignment Two

## AUTOMATING REVIEWS IN MEDICINE

# Load the dataset

trials = read.csv("./data/clinical_trial.csv", stringsAsFactors=FALSE)

str(trials)
summary(trials)

#1.1
max(nchar(trials[,2]))

#1.2 - how many with no abstract
nrow(subset(trials,nchar(trials[,2])==0))

#1.3  which variable has shortest title?
trials[nchar(trials$title)==min(nchar(trials$title)),1]

## PREPARING THE CORPUS

#2.1

library(tm)
#install.packages("SnowballC")

#The title

# Create corpus
corpusTitle = Corpus(VectorSource(trials$title))
# Pre-process data
corpusTitle = tm_map(corpusTitle, tolower)
# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
str(dtmTitle)
# Remove sparse terms
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
# Create data frame
dtmTitle = as.data.frame(as.matrix(dtmTitle))

#The Abstract

# Create corpus
corpusAbstract = Corpus(VectorSource(trials$abstract))
# Pre-process data
corpusAbstract = tm_map(corpusAbstract, tolower)
# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmAbstract = DocumentTermMatrix(corpusAbstract)
str(dtmAbstract)
# Remove sparse terms
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
# Create data frame
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

# number of terms in dtmTitle = number of columns
ncol(dtmTitle)
# number of terms in dtmAbstract = number of columns
ncol(dtmAbstract)

# 2.3 most frequent word stem in abstracts
library(dplyr)
sums<-as.numeric(colSums(dtmAbstract))
names<-names(dtmAbstract)
Absums<-data.frame(names,sums)
arrange(Absums,-sums)[1,]

## BUILDING A MODEL

#3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

#3.2
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial=trials$trial
ncol(dtm)

# Split the data

library(caTools)

set.seed(144)
spl = sample.split(dtm$trial, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

# baseline accuracy on test set
table(test$trial)
table(test$trial)[1]/sum(table(test$trial))

# 1.6 Build a CART model and find accuracy on test set
# Build a CART model

library(rpart)
library(rpart.plot)
trialCART = rpart(trial~., data=train, method="class")

# 3.4 plot the CART tree
prp(trialCART)

#3.5 training set predictions
predTrain = predict(trialCART)
predTrain [1:10,]
pred.prob.Train  = predTrain[,2]
max(pred.prob.Train)

# 3.7 Accuracy on training set
ct<-table(train$trial,pred.prob.Train >= 0.5)
ct
sum(diag(ct))/sum(ct)

#Sensitivty on training set
ct[2,2]/(ct[2,1]+ct[2,2])

#specificity on training set
ct[1,1]/(ct[1,1]+ct[1,2])
               
## EVALUATING THE MODEL ON THE TESTING SET

# Make predictions on the test set
pred = predict(trialCART, newdata=test)
pred[1:10,]
pred.prob = pred[,2]

# 4.1 Compute accuracy on test set
ct<-table(test$trial,pred.prob >= 0.5)
ct
sum(diag(ct))/sum(ct)

#AUC of prediction model
library(ROCR)

#Predict AUC of the prediction model

pred = prediction(pred.prob, test$trial)
as.numeric(performance(pred, "auc")@y.values)
perfC = performance(pred, "tpr", "fpr")
plot(perfC)
