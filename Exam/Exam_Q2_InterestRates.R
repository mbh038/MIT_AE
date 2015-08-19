## The Analytics Edge

##  MITx 15.071x

## Michael Hunt
## August 2015

## Exam Question Two

## Forecasting Interest Rate Hikes by Federal Reserve

## P1 LOADING THE DATA

# download data into R
fedFunds<-read.csv("./data/federalFundsRate.csv",stringsAsFactors=FALSE)

# inspect the data
str(fedFunds)
summary(fedFunds)

# What proportion of months did the Fed raise the interest rate?
mean(fedFunds$RaisedFedFunds)

## PROBLEM 2: THE LONGEST-SERVING FED CHAIR
table(fedFunds$Chairman)

## PROBLEM 3 - CONVERTING VARIABLES TO FACTORS
fedFunds$Chairman<-as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres<-as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds<-as.factor(fedFunds$RaisedFedFunds)
str(fedFunds)

## PROBLEM 4 - SPLITTING INTO A TRAINING AND TESTING SET
set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)
training = subset(fedFunds, spl==TRUE)
testing = subset(fedFunds, spl==FALSE)

## PROBLEM 5 - TRAINING A LOGISTIC REGRESSION MODEL
LRModel<-glm(RaisedFedFunds~PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection,family=binomial,data=training)
summary(LRModel)

## PROBLEM 6 - PREDICTING USING A LOGISTIC REGRESSION MODEL

#You know that the rate has been lowered for 3 straight months 

LRModel$coefficients
conditions<-c(1,1.7,-3,5.1,65.3,0,18)
logOdds<-sum(LRModel$coefficients*conditions)

PRaise<-1/(1+exp(-logOdds))
PRaise

## PROBLEM 7 - INTERPRETING MODEL COEFFICIENTS
exp(LRModel$coefficients[6])

## PROBLEM 8 - OBTAINING TEST SET PREDICTIONS

predictTest = predict(LRModel, type="response", newdata=testing)
# Confusion matrix for threshold of 0.5
ctTest<-table(testing$RaisedFedFunds, predictTest> 0.5)
ctTest

#accuracy
(sum(diag(ctTest)))/sum(ctTest)

# baseline accuracy
# most common outcome for traing set is:
nrow(subset(training,training$RaisedFedFunds=="1"))/nrow(training)
# this >0.5 (just) so baseline accuracy on test set predicts rate rise in every case
# hence number of times where our prediction model makes different predictions to this is:
sum(ctTest[,1]) # = sum of al times where we predict no rate rise (FALSE)

## PROBLEM 9 - COMPUTING TEST-SET AUC

library(ROCR)
ROCRpredTest = prediction(predictTest, testing$RaisedFedFunds)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

## PROBLEM 12 - ROC CURVES

# Performance function
ROCRperf = performance(ROCRpredTest, "tpr", "fpr")

# plot ROC curve
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

## PROBLEM 13 - CROSS-VALIDATION TO SELECT PARAMETERS

## PROBLEM 14 - CROSS-VALIDATION FOR A CART MODEL


#4.1
library(caret)
library(e1071)
library(ROCR)
library(rpart)
library(rpart.plot)
#RaisedFedFunds" using the independent variables "PreviousRate," "Streak,"
# "Unemployment," "HomeownershipRate," "DemocraticPres," and 
# "MonthsUntilElection." Select the cp value from a grid consisting
# of the 50 values 0.001, 0.002, ..., 0.05.

# Number of folds
set.seed(201)
tr.control = trainControl(method = "cv", number = 10)

#Test cp values from 0.002 to 0.1 in 0.002 increments
cartGrid = expand.grid( .cp = seq(0.001,0.05,0.001))
cartGrid

# Cross-validation
tr = train(RaisedFedFunds ~ PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data = training, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr

# Extract tree
best.tree = tr$finalModel
prp(best.tree)

## PROBLEM 17 - TEST-SET ACCURACY FOR CART MODEL

set.seed(201)
CARTcp = rpart(RaisedFedFunds ~ PreviousRate+Streak+Unemployment+HomeownershipRate+DemocraticPres+MonthsUntilElection, data=training, method="class",cp=0.016)
prp(CARTcp)

# accuracy
PredictCARTcp = predict(CARTcp, newdata = testing, type = "class")
ctcp<-table(testing$RaisedFedFunds, PredictCARTcp)
ctcp
sum(diag(ctcp))/sum(ctcp)
