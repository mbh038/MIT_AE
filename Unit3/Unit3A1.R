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
model1<-glm(violator~.,family=binomial,data=train)
summary(model1)

##4.3 - BUILDING A LOGISTIC REGRESSION MODEL
#Consider a parolee who is male, of white race, aged 50 years at prison release, 
#from the state of Maryland, served 3 months, had a maximum sentence of 12 months,
#did not commit multiple offenses, and committed a larceny. Answer the following
#questions based on the model's predictions for this individual.
(HINT: You should use the coefficients of your model, the Logistic Response Function,
and the Odds equation to solve this problem.)

#According to the model, what are the odds this individual is a violator?
parolee1<-c(1,1,50,1,3,12,0,2)
logitp1<-
