Unit3A3.R

## PREDICTING REPAYMENT OF LOANS

## UNDERSTANDING THE DATA

loans<-read.csv("./data/loans.csv")
str(loans)
summary(loans)
#head(songs)

## PREPARING THE DATASET

#1.1
sum(loans$not.fully.paid)/nrow(loans)

#1.4
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

#need to do this instead to get profit of 6.2 right ??
#loans<-read.csv("./data/loans_imputed.csv")

##PREDICTION MODELS

#2.1 split into testing and training
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

#glm model on all independent variables. 
#Which are significant?
model1<-glm(not.fully.paid~.,family=binomial,data=train)
summary(model1)

#2.2
#A:fico 700; B:fico 710
#ratio of l;og odds
(700-710)*model1$coef["fico"]
#ratio of odds (odds A / odds B)
exp((700-710)*model1$coef["fico"])

#2.3
test$predicted.risk = predict(model1, type="response", newdata=test)

ct<-table(test$not.fully.paid, test$predicted.risk > 0.5)
ct

#specificity
ct[1,1]/(ct[1,1]+ct[1,2])

#sensitivity
ct[2,2]/(ct[2,1]+ct[2,2])

#accuracy
(ct[1,1]+ct[2,2])/sum(ct)

#baseline accuracy
(ct[1,1]+ct[1,2])/sum(ct)

#2.4 Use the ROCR package to compute the test set AUC.
library(ROCR)
ROCRpred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

##A "SMART BASELINE"
#3.1 - use just int.rate as predictor = bivariate model (bv)
model2<-glm(not.fully.paid~int.rate,family=binomial,data=train)
summary(model2)

#3.2
test$predicted.risk.bv = predict(model2, type="response", newdata=test)
max(test$predicted.risk.bv)

ct<-table(test$not.fully.paid, test$predicted.risk.bv > 0.5)
ct

#3.3 AUC of bivariate model
ROCRpred = prediction(test$predicted.risk.bv, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

##4.1 COMPUTING THE PROFITABILITY OF AN INVESTMENT
c<-10
r<-6/100
t<-3
c*exp(r*t)

##5.1 A SIMPLE INVESTMENT STRATEGY
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

10*max(test$profit)

#6.1 - AN INVESTMENT STRATEGY BASED ON RISK
#invest $100 evenly split between all loans - profit is:
sum((100/nrow(test))*test$profit)

#pick out the high interest loans
highInterest<-subset(test,test$int.rate>=0.15)
#average profit on $1 investment
mean(highInterest$profit)
#proportion of these that do not pay back fully
-sum(highInterest$profit[highInterest$not.fully.paid == 1])/nrow(highInterest)

#6.2
#among these, select the 100 loans predicted to have lowest risk
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans<-subset(highInterest,highInterest$predicted.risk<=cutoff)
nrow(selectedLoans)


#profit if invest $1 in each of these loans
##FOR THIS PART, READ LOANS_IMPUTED in 1.4
sum(selectedLoans$profit)
##FOR THIS PART, RUN MICE in 1.4
#how many of these do not pay back?
-sum(selectedLoans$profit[selectedLoans$not.fully.paid == 1])
