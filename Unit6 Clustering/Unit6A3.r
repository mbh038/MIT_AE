## THE ANALYTICS EDGE 15.071x

## UNIT 6 A3

## Michael Hunt

## July 2015

## PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT

# ReturnJan = the return for the company's stock during January (in the year of the observation). 
# ReturnFeb = the return for the company's stock during February (in the year of the observation). 
# ReturnMar = the return for the company's stock during March (in the year of the observation). 
# ReturnApr = the return for the company's stock during April (in the year of the observation). 
# ReturnMay = the return for the company's stock during May (in the year of the observation). 
# ReturnJune = the return for the company's stock during June (in the year of the observation). 
# ReturnJuly = the return for the company's stock during July (in the year of the observation). 
# ReturnAug = the return for the company's stock during August (in the year of the observation). 
# ReturnSep = the return for the company's stock during September (in the year of the observation). 
# ReturnOct = the return for the company's stock during October (in the year of the observation). 
# ReturnNov = the return for the company's stock during November (in the year of the observation). 
# PositiveDec = whether or not the company's stock had a positive return in December (in the year of the observation).
#   This variable takes value 1 if the return was positive, and value 0 if the return was not positive.

# For the first 11 variables, the value stored is a proportional change in stock value during that month.
# For instance, a value of 0.05 means the stock increased in value 5% during the month, while a value 
# of -0.02 means the stock decreased in value 2% during the month.

# load data into R
stocks<-read.csv("./data/StocksCluster.csv")
str(stocks)
summary(stocks)

# proportion of stocks giving positive returns in December
mean(stocks$PositiveDec)

# pairwise correlations between all the Return variables
round(cor(stocks[,1:11]),3)

library(Matrix)
max(cor(stocks[,1:11])-Diagonal(11))

summary(stocks)

## INITIAL LOGISTIC REGRESSION MODEL

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel<-glm(PositiveDec~.,family=binomial,data=stocksTrain)

# accuracy on training set
predictLog = predict(StocksModel,data=stocksTrain,type="response")
max(predictLog)
ct<-table(stocksTrain$PositiveDec, predictLog >= 0.5)
ct
(sum(diag(ct)))/sum(ct)

# accuracy on test set
predictLogTest = predict(StocksModel,newdata=stocksTest,type="response")
max(predictLogTest)
ctTest<-table(stocksTest$PositiveDec, predictLogTest >= 0.5)
ctTest
(sum(diag(ctTest)))/sum(ctTest)

# baseline accuracy on test set - always predict most common outcome :PositiveDec = 1
mean(stocksTest$PositiveDec) # check this IS the most positve outcome
(ctTest[2,1]+ctTest[2,2])/sum(ctTest)

## CLUSTERING STOCKS

# First, remove dependent variable from training and test sets
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# normalise the data - has effect of subtracting the mean, then dividing by st dev
# of variables in train set. Can use preProcess for this:
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

mean(normTrain$ReturnJan) # is zero, since we normalised on the train set
mean(normTest$ReturnJan) # is not zero

# Do k-means clustering
# Specify number of clusters
k = 3
# Run k-means on training set
set.seed(144)
km = kmeans(normTrain, centers = k, iter.max = 1000)
str(km)
summary(km)
table(km$cluster)

# use the flexclust package to obtain training set and testing set
# cluster assignments for our observations - as in recitation
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTrain) # same numbers in each cluster as in km$cluster
table(clusterTest)

## CLUSTER-SPECIFIC PREDICTIONS

# separate elements of data sets by the clusters to which they have been assigned
stocksTrain1<-subset(stocksTrain,clusterTrain==1)
nrow(stocksTrain1)
stocksTrain2<-subset(stocksTrain,clusterTrain==2)
nrow(stocksTrain2)
stocksTrain3<-subset(stocksTrain,clusterTrain==3)
nrow(stocksTrain3)
stocksTest1<-subset(stocksTest,clusterTest==1)
nrow(stocksTest1)
stocksTest2<-subset(stocksTest,clusterTest==2)
nrow(stocksTest2)
stocksTest3<-subset(stocksTest,clusterTest==3)
nrow(stocksTest3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# logistic regression
stocksModel1<-glm(PositiveDec~.,family=binomial,data=stocksTrain1)
summary(stocksModel1)$coef
stocksModel2<-glm(PositiveDec~.,family=binomial,data=stocksTrain2)
summary(stocksModel2)$coef
stocksModel3<-glm(PositiveDec~.,family=binomial,data=stocksTrain3)
summary(stocksModel3)$coef

# test set predictions of logistic regression models using threshold 0.5

# cluster 1
predictLogTest1 = predict(stocksModel1,newdata=stocksTest1,type="response")
#max(predictLogTest1)
ctTest1<-table(stocksTest1$PositiveDec, predictLogTest1 >= 0.5)
ctTest1
(sum(diag(ctTest1)))/sum(ctTest1)

# cluster 2
predictLogTest2 = predict(stocksModel2,newdata=stocksTest2,type="response")
#max(predictLogTest2)
ctTest2<-table(stocksTest2$PositiveDec, predictLogTest2 >= 0.5)
ctTest2
(sum(diag(ctTest2)))/sum(ctTest2)

# cluster 3
predictLogTest3 = predict(stocksModel3,newdata=stocksTest3,type="response")
#max(predictLogTest3)
ctTest3<-table(stocksTest3$PositiveDec, predictLogTest3 >= 0.5)
ctTest3
(sum(diag(ctTest3)))/sum(ctTest3)

# To compute the overall test-set accuracy of the cluster-then-predict approach,
# we can combine all the test-set predictions into a single vector and all the
# true outcomes into a single vector:

AllPredictions = c(predictLogTest1, predictLogTest2, predictLogTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

ctAll<-table(AllOutcomes,AllPredictions >=0.5)
ctAll
(sum(diag(ctAll)))/sum(ctAll)

# a modest improvement over logistic regression model accuracy without first clustering.

