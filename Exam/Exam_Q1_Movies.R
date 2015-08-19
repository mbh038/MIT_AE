## The Analytics Edge

##  MITx 15.071x

## Michael Hunt
## August 2015

## Exam Question One

## Movies

## P1 LOADING THE DATA

# download data into R
Movies<-read.csv("./data/Movies.csv")

# inspect the data
str(Movies)
summary(Movies)

# create training and test sets
# training: all movies released before 2010
# testing: all movies released in 2010 and after

MoviesTrain = subset(Movies,Movies$Year<2010)
MoviesTest = subset(Movies,Movies$Year>=2010)

str(MoviesTrain)
str(MoviesTest)

summary(MoviesTrain$Year)
summary(MoviesTest$Year)

nrow(MoviesTrain)
nrow(MoviesTest)

## P2 METHOD OF SPLITTING THE DATA
library(caTools)
set.seed(123)
?sample.split 
#- typically used to split data with a categorical dependent variable
# , as for a classification problem, and we have a continuous dependent
# variable.

## P3 A LINEAR REGRESSION MODEL
RegModel<-lm(Worldwide~., data = MoviesTrain[ , 3:ncol(MoviesTrain)])
summary(RegModel)

## PROBLEM 4 - CHECKING FOR SIGNIFICANCE

## PROBLEM 5 - CORRELATIONS
cor(MoviesTrain$Worldwide,MoviesTrain$Production.Budget)

## PROBLEM 6 - AN UPDATED MODEL
# include only significant variables from first model
RegModel<-lm(Worldwide~Runtime+Crime+Horror+Animation+History+Nominations+Production.Budget, data = MoviesTrain)
summary(RegModel)

## PROBLEM 7 - UNDERSTANDING COEFFICIENTS

## PROBLEM 8 - PREDICTIONS ON THE TEST SET
RegModel.pred = predict(RegModel, newdata=MoviesTest)
SSE = sum((RegModel.pred - MoviesTest$Worldwide)^2)
SSE
RMSE = sqrt(SSE/nrow(MoviesTest))
RMSE

# baseline score
baseline<-mean(MoviesTrain$Worldwide)
baseline
SST=sum((MoviesTest$Worldwide-baseline)^2)
SST

# Rsq on test set
RSQ<-1-SSE/SST
RSQ

## PROBLEM NINE - UNDERSTANDING THE MODEL
# this value is higher than for the train set, so model does not
# suffer from overfitting.

## PROBLEM 10 - A CLASSIFICATION PROBLEM  

Movies$Performance = factor(ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .75), "Excellent", ifelse(Movies$Worldwide > quantile(Movies$Worldwide, .25), "Average", "Poor")))
summary(Movies$Performance)
str(Movies$Performance)
Movies$Worldwide = NULL

# Split the data
library(caTools)
set.seed(15071)
split = sample.split(Movies$Performance, SplitRatio = 0.7)
MoviesTrain = subset(Movies, split==TRUE)
MoviesTest = subset(Movies, split==FALSE)

## PROBLEM 11 - A CART MODEL
# Load CART packages
library(rpart)
library(rpart.plot)

CART_Model = rpart(Performance ~ ., data = MoviesTrain[ , 3:ncol(MoviesTrain)], method="class")
prp(CART_Model)
# 9 splits

## PROBLEM 12 - TRAINING SET ACCURACY
PredictCART = predict(CART_Model, newdata = MoviesTrain, type = "class")
ct<-table(MoviesTrain$Performance, PredictCART)
(sum(diag(ct)))/sum(ct)

## PROBLEM 13 - A BASELINE MODEL
sum(ct[1,])/sum(ct)

## PROBLEM 14 - TESTING SET ACCURACY
PredictCARTtest = predict(CART_Model, newdata = MoviesTest, type = "class")
ctTest<-table(MoviesTest$Performance, PredictCARTtest)
(sum(diag(ctTest)))/sum(ctTest)

## PROBLEM 15 - BASELINE ACCURACY ON TESTING SET
sum(ctTest[1,])/sum(ctTest)
