## Parole

## Unit 3 assessment 1

parole<-read.csv("./data/parole.csv")
str(parole)
summary(parole)

##1.2
sum(parole$violator)

##2.2 # change to factors whre necessary
parole$state<-as.factor(parole$state)
parole$crime<-as.factor(parole$crime)
str(parole)
summary(parole)

##3.1 split into testing and training
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

##4.1 - BUILDING A LOGISTIC REGRESSION MODEL
model1<-glm(violator~.-1,family=binomial,data=train) # no intercept
summary(model1)

##4.3 - BUILDING A LOGISTIC REGRESSION MODEL
#Consider a parolee who is male, of white race, aged 50 years at prison release, 
#from the state of Maryland, served 3 months, had a maximum sentence of 12 months,
#did not commit multiple offenses, and committed a larceny. Answer the following
#questions based on the model's predictions for this individual.
#(HINT: You should use the coefficients of your model, the Logistic Response Function,
#and the Odds equation to solve this problem.)

#According to the model, what are the odds this individual is a violator?
parolee1<-c(1,1,50,1,0,0,0,3,12,0,1,0,0) # assuming no intercept
logitp1<-sum(model1$coef*parolee1)
oddsViolator<-exp(logitp1)
oddsViolator
probViolator<-oddsViolator/(1+oddsViolator)
probViolator

## 5.1 - EVALUATING THE MODEL ON THE TESTING SET
predictTest = predict(model1, type="response", newdata=test)
max(predictTest)

## 5.2 - EVALUATING THE MODEL ON THE TESTING SET
# Confusion matrix with threshold of 0.5
ct<-table(test$violator, predictTest > 0.5)
ct

#specificity
ct[1,1]/(ct[1,1]+ct[1,2])

#sensitivity
ct[2,2]/(ct[2,1]+ct[2,2])

#accuracy
(ct[1,1]+ct[2,2])/sum(ct)

## 5.3 - EVALUATING THE MODEL ON THE TESTING SET

#baseline accuracy - predict every parolee is a non-violator
(ct[1,1]+ct[1,2])/sum(ct)

## 5.4 - EVALUATING THE MODEL ON THE TESTING SET
ct<-table(test$violator, predictTest > 0.7)
ct
ct[2,2]/(ct[2,1]+ct[2,2])
ct<-table(test$violator, predictTest > 0.5)
ct
ct[2,2]/(ct[2,1]+ct[2,2])
ct<-table(test$violator, predictTest > 0.3)
ct
ct[2,2]/(ct[2,1]+ct[2,2])

## 5.6 - EVALUATING THE MODEL ON THE TESTING SET
#Using the ROCR package, what is the AUC value for the model?

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)


## 5.7 - EVALUATING THE MODEL ON THE TESTING SET
# Describe the meaning of AUC in this context.
