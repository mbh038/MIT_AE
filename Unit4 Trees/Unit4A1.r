##Unit 4 Assignment 1

# Understanding why people vote

# Read in data
gerber = read.csv("./data/gerber.csv")
str(gerber)

##EXPLORATION AND LOGISTIC REGRESSION

#1.1 proportion that voted
mean(gerber$voting)

#1.2 proportion voting within groups

mean(gerber$voting[gerber$hawthorne==1])
mean(gerber$voting[gerber$civicduty==1])
mean(gerber$voting[gerber$neighbors==1])
mean(gerber$voting[gerber$self==1])

#1.3 logistic regression model on whole data set
model1<-glm(voting~hawthorne+civicduty+neighbors+self,family=binomial,data=gerber)
summary(model1)

#Accuracy of model 
predict1 = predict(model1, type="response")
max(predict1)

#1.4 accuracy with threshold 0.3
ct<-table(gerber$voting, predict1 >= 0.3)
ct
(ct[1,1]+ct[2,2])/sum(ct)

#1.5 accuracy with threshold 0.5
ct<-table(gerber$voting, predict1 >= 0.5)
ct
ct[1,1]/sum(ct)

#baseline accuracy = proportion that did not vote
1-mean(gerber$voting)

library(ROCR)
ROCRpredTest = prediction(predict1, gerber$voting)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

#AUC is low, so even though all parameters are significant, model 
# is weakly predictive - since meaning of AUC is ability of model
# to correctly predict voting intentions of randomly selected voter

## 2 TREES

#2.1
# Load CART packages
library(rpart)
library(rpart.plot)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel) # see no splitting

#2.2 - add complexity parameter = 0 to force splitting
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#2.3 - include sex as a variable, keep cp=0
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors+sex, data=gerber, cp=0.0)
prp(CARTmodel3)

## 3 INTERACTION TERMS
#3.1
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4,digits=6)
abs(0.296638-0.34)
CARTmodel5 = rpart(voting ~ control+sex, data=gerber, cp=0.0)
prp(CARTmodel5,digits=6)

#3.3
model6<-glm(voting~sex+control,family=binomial,data=gerber)
summary (model6)

#3.4
# ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) )
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model6, newdata=Possibilities, type="response")
0.290456-predict(model6, newdata=Possibilities, type="response")[4]


#3.5
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary (LogModel2)

#3.6
# ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) )
predict(LogModel2, newdata=Possibilities, type="response")
0.290456-predict(LogModel2, newdata=Possibilities, type="response")[4]
