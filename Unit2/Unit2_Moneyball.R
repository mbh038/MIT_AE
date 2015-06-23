# VIDEO 2

#load packages
library (ggplot2)
library(dplyr)

# Read in data
baseball = read.csv("./data/baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

#QQ
#If a baseball team scores 713 runs and allows 614 runs, how many games do we 
#expect the team to win?
WinsReg$coef[1]+WinsReg$coef[2]*(713-614)

# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

#QQ
#If a baseball team's OBP is 0.311 and SLG is 0.405,
#how many runs do we expect the team to score?

RunsReg$coef
RunsReg$coef[1]+RunsReg$coef[2]*0.311+RunsReg$coef[3]*0.405

#If a baseball team's opponents OBP (OOBP) is 0.297 and oppenents SLG (OSLG)
# is 0.370, how many runs do we expect the team to allow?

RAReg = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RAReg)
RAReg$coef[1]+RAReg$coef[2]*.297+RAReg$coef[3]*.37

#QQ

#Suppose you are the General Manager of a baseball team, and you are selecting
#TWO players for your team. You have a budget of $1,500,000, and you have the
#choice between the following players:
        
#prices in M$
playerStats<-data.frame("OBP"=c(0.338,.391,.369,.313,.361),"SLG"=c(0.54,0.45,.374,.447,.500),"Price"=c(1.4,1.065,.295,.8,.3))
playerStats

RunsReg$coef[1]+RunsReg$coef[2]*playerStats[1,1]+RunsReg$coef[3]*playerStats[1,2]
RunsReg$coef[1]+RunsReg$coef[2]*playerStats[2,1]+RunsReg$coef[3]*playerStats[2,2]
RunsReg$coef[1]+RunsReg$coef[2]*playerStats[3,1]+RunsReg$coef[3]*playerStats[3,2]
RunsReg$coef[1]+RunsReg$coef[2]*playerStats[4,1]+RunsReg$coef[3]*playerStats[4,2]
RunsReg$coef[1]+RunsReg$coef[2]*playerStats[5,1]+RunsReg$coef[3]*playerStats[5,2]

#QQ

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012<-c(94,88,95,88,93,94,98,97,93,94)
wins2013<-c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
cor(teamRank,wins2013)
