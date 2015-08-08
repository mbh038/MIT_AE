##Unit 4 Assignment 2

# Letter Recognition

# Read in data
letters = read.csv("./data/letters_ABPR.csv")
str(letters)

## PREDICTING B OR NOT B

letters$isB = as.factor(letters$letter == "B")

# Split the data
library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl==TRUE)
test = subset(letters, spl==FALSE)

#baseline accuracy
# = proportion of not B in test set - the most frequent class
1-mean(as.numeric(test$isB)-1)

#1.2
#load CART packages
library(rpart)
library(rpart.plot)

CARTb = rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)

#accuracy of CART model on test set
PredictCARTb = predict(CARTb, newdata = test, type = "class")
ct<-table(test$isB, PredictCARTb)
ct
(ct[1,1]+ct[2,2])/sum(ct)

# Install randomForest package
#install.packages("randomForest")
library(randomForest)

letterForest = randomForest(isB ~ .-letter, data = train )

PredictForest = predict(letterForest, newdata = test)
ctab<-table(test$isB, PredictForest)
ctab
accuracy<-(ctab[1,1]+ctab[2,2])/sum(ctab)
accuracy

# PREDICTING THE LETTERS A, B, P, R

#ie building a multiclass classification CART model in R cf the D2Hawkeye example

letters$letter = as.factor( letters$letter )

# Split the data based on letters
library(caTools)
set.seed(2000)
spl2 = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, spl2==TRUE)
test2 = subset(letters, spl2==FALSE)

#2.1
#baseline model - predicts that all letters are the most common one which is:
table(train2$letter) # P
#baseline accuracy on training set
max(table(train2$letter))/sum(table(train2$letter))
#now try on test set
table(test2$letter) # P
#baseline accuracy on testing set
max(table(test2$letter))/sum(table(test2$letter))

#2.2 
#Now build a classification tree to predict "letter", using the training set to
# build your model.You should use all of the other variables as independent 
# variables, except "isB", since it is related to what we are trying to predict! 
# Just use the default parameters in your CART model. 
# Add the argument method="class" since this is a classification problem. 
# Even though we have multiple classes here, nothing changes in how we build 
# the model from the binary case.

library(rpart)
library(rpart.plot)

CARTl = rpart(letter ~ . -isB, data=train2, method="class")
prp(CARTl)

#accuracy of CARTl model on test2 set
PredictCARTl = predict(CARTl, newdata = test2, type = "class")
ctl<-table(test2$letter, PredictCARTl)
ctl
sum(diag(ctl))/sum(ctl)

#random forest model on A, B, P Q prediction
library(randomForest)

letterForest2 = randomForest(letter ~ .-isB, data = train2 )

PredictForest = predict(letterForest2, newdata = test2)
ctlrf<-table(test2$letter, PredictForest)
ctlrf
#accuracy on test set
sum(diag(ctlrf))/sum(ctlrf)
