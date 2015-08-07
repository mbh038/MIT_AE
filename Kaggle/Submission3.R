## KAGGLE COMPETITION

## S2 - Random Forest

##
##  A RANDOM FOREST MODEL

# Read in data
train = read.csv("./data/eBayiPadTrain.csv",stringsAsFactors=FALSE,na.strings="Unknown")
test = read.csv("./data/eBayiPadTest.csv", stringsAsFactors=FALSE,na.strings="Unknown")
train=na.omit(train)

str(train)

mean(train$sold)  # is <0.5 - so most are not sol


# 3.1
set.seed(1)
trainSmall = train[sample(nrow(train), 200), ]

library(randomForest)
set.seed(1)
soldForest = randomForest(sold ~ biddable+startprice+cellular+storage, data = trainSmall )

PredictForest = predict(soldForest, newdata = train)
ctrf<-table(train$sold, PredictForest)
ctrf

#accuracy on train set
sum(diag(ctrf))/sum(ctrf)

# 3.2 assessing importance of variables
# - by number times used to initiate a split: more => more important
vu = varUsed(soldForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(soldForest$forest$xlevels[vusorted$ix]))

# 3.3 assessing importance of variables
# by average reduction in impurity
varImpPlot(soldForest)

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
tr = train(sold ~ biddable+startprice+condition+storage+productline, data = train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr

# Extract tree
best.tree = tr$finalModel
prp(best.tree)

# fit CART model with optimal cp = 0.002
library(rpart)
library(rpart.plot)

CARTcp = rpart(sold ~biddable+startprice+condition+storage+productline, data=train, method="class",cp=0.002)
prp(CARTcp)

# 2.4 accuracy
PredictCARTcp = predict(CARTcp, newdata = train, type = "class")
ctcp<-table(train$sold, PredictCARTcp)
sum(diag(ctcp))/sum(ctcp)
##

library(ROCR)
predRF = prediction(as.numeric(PredictCARTcp), train$sold)
as.numeric(performance(predRF, "auc")@y.values)

PredictCARTcptest = predict(CARTcp, newdata = test, type = "class")

MySubmission3 = data.frame(UniqueID = test$UniqueID, Probability1 = PredictCARTcptest)

write.csv(MySubmission3, "./submissions/SubmissionSimplecartcp.csv", row.names=FALSE)


# biddable+startprice+condition+cellular+carrier+color+storage+productline

train AUC - 0.8652028
test AUC  - 0.83413

accuracy - 0.8119291


## LOGISTIC REGRESSION MODEL


# 1.1 Use training set to build logistic "Sold", yes or no, predictive model
modelLog<-glm(sold~biddable+startprice+condition+cellular+carrier+color+storage+productline,family=binomial,data=train)
summary(modelLog)

# 1.2 What is the accuracy of the model on the testing set? 
# Use a threshold of 0.5. 
predictLog = predict(modelLog,newdata=train)
max(predictLog)
ct<-table(train$sold, predictLog >= 0.5)
ct
(sum(diag(ct)))/sum(ct)

# 1.3 What is the baseline accuracy for the testing set?
# = accuracy of the prediction always that iPad not sold.
sum(ct[1,])/sum(ct)

# What is the area-under-the-curve (AUC) for this model on the test set?
# ROC curve

library(ROCR)
predLog = prediction(predictLog, train$sold)
as.numeric(performance(predLog, "auc")@y.values)

perfLog = performance(predLog, "tpr", "fpr")
plot(perfLog)

# And then make predictions on the test set:

PredTest = predict(modelLog, newdata=test, type="response")

# We can't compute the accuracy or AUC on the test set ourselves, since we don't have the dependent variable on the test set (you can compute it on the training set though!). 
# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission2 = data.frame(UniqueID = test$UniqueID, Probability1 = PredTest)

write.csv(MySubmission2, "./submissions/SubmissionSimpleLog2.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

