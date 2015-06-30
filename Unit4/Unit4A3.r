##Unit 4 Assignment 3

# PREDICTING EARNINGS FROM CENSUS DATA

# In this problem, we are going to use census information about an individual
# to predict how much a person earns -- in particular, whether the person 
# earns more than $50,000 per year. This data comes from the 

# Read in data
census = read.csv("./data/census.csv")
str(census)

## LOGISTIC REGRESSION MODEL

# Split the data
library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)

# 1.1 Use training set to build logistic >50k, yes or no, predictive model
modelLog<-glm(over50k~.,family=binomial,data=train)
summary(modelLog)

# 1.2 What is the accuracy of the model on the testing set? 
# Use a threshold of 0.5. 
predictLog = predict(modelLog,newdata=test)
max(predictLog)
ct<-table(test$over50k, predictLog >= 0.5)
ct
(sum(diag(ct)))/sum(ct)

# 1.3 What is the baseline accuracy for the testing set?
# = accuracy of the prediction always that salary <50k.
sum(ct[1,])/sum(ct)

# What is the area-under-the-curve (AUC) for this model on the test set?
# ROC curve

library(ROCR)
predLog = prediction(predictLog, test$over50k)
as.numeric(performance(predLog, "auc")@y.values)

perfLog = performance(predLog, "tpr", "fpr")
plot(perfLog)

## CART MODEL

# 2.1
library(rpart)
library(rpart.plot)

CARTb = rpart(over50k ~., data=train, method="class")
prp(CARTb)

# 2.4 accuracy
PredictCART = predict(CARTb, newdata = test, type = "class")
ct<-table(test$over50k, PredictCART)
sum(diag(ct))/sum(ct)

# 2.5 CART ROC curve and AUC of CART model

library(ROCR)

#PredictROC
pred = prediction(PredictCART[,2], test$over50k)
as.numeric(performance(pred, "auc")@y.values)

perfC = performance(pred, "tpr", "fpr")
plot(perfC)

##  A RANDOM FOREST MODEL

# 3.1
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

library(randomForest)
set.seed(1)
over50kForest = randomForest(over50k ~ ., data = trainSmall )

PredictForest = predict(over50kForest, newdata = test)
ct50rf<-table(test$over50k, PredictForest)
ct50rf

#accuracy on test set
sum(diag(ct50rf))/sum(ct50rf)

# 3.2 assessing importance of variables
# - by number times used to initiate a split: more => more important
vu = varUsed(over50kForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(over50kForest$forest$xlevels[vusorted$ix]))

# 3.3 assessing importance of variables
# by average reduction in impurity
varImpPlot(over50kForest)

## SELECTING CP BY CROSS-VALIDATION

#4.1
library(caret)
library(e1071)

# Number of folds
set.seed(2)
tr.control = trainControl(method = "cv", number = 10)

#Test cp values from 0.002 to 0.1 in 0.002 increments
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

# Cross-validation
tr = train(over50k ~ ., data = train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr

# Extract tree
best.tree = tr$finalModel
prp(best.tree)

# fit CART model with optimal cp = 0.002
library(rpart)
library(rpart.plot)

CARTcp = rpart(over50k ~., data=train, method="class",cp=0.002)
prp(CARTcp)

# 2.4 accuracy
PredictCARTcp = predict(CARTcp, newdata = test, type = "class")
ctcp<-table(test$over50k, PredictCARTcp)
sum(diag(ctcp))/sum(ctcp)
