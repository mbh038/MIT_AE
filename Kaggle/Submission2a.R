## KAGGLE COMPETITION

## S2a - Logistic regression - significant variables only

# biddable+startprice+condition+storage+productline

train AUC - 0.8626658
test AUC  - 0.83625

accuracy - 0.8162278

# Read in data
train = read.csv("./data/eBayiPadTrain.csv",stringsAsFactors=FALSE,na.strings="Unknown")
test = read.csv("./data/eBayiPadTest.csv", stringsAsFactors=FALSE,na.strings="Unknown")

train=na.omit(train)

str(train)

mean(train$sold)  # is <0.5 - so most are not sol

## LOGISTIC REGRESSION MODEL


# 1.1 Use training set to build logistic "Sold", yes or no, predictive model
modelLog<-glm(sold~biddable+startprice+condition+cellular+carrier+color+storage+productline,family=binomial,data=train)
summary(modelLog)

# keep only significant variables
modelLog<-glm(sold~biddable+startprice+condition+storage+productline,family=binomial,data=train)
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

MySubmission2a = data.frame(UniqueID = test$UniqueID, Probability1 = PredTest)

write.csv(MySubmission2a, "./submissions/SubmissionSimpleLog2a.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

