# Set working directory
setwd("C:/Users/Mike/Rspace/MIT_AE/Unit2")

library(dplyr)
library(lubridate)

## READING TEST SCORES

if(!file.exists("data")){
        dir.create("data")
}

# download train data if not yet already done so
if(!file.exists("./data/pisa2009train.csv")){
        
        #download a data file as csv into the "data" dir
        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/pisa2009train.csv"
        download.file(fileURL,destfile="./data/pisa2009train.csv")
        #include date of download
        
}
pisaTrain<-read.csv("./data/pisa2009train.csv")

# download test data if not yet already done so
if(!file.exists("./data/pisa2009test.csv")){
        
        #download a data file as csv into the "data" dir
        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/pisa2009test.csv"
        download.file(fileURL,destfile="./data/pisa2009test.csv")
        #include date of download
        
}
pisaTest<-read.csv("./data/pisa2009test.csv")

#1.1
str(pisaTrain)

#1.2
tapply(pisaTrain$readingScore,pisaTrain$male,mean)

#1.4
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

nrow(pisaTrain)
nrow(pisaTest)

#3.1
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore<-lm(readingScore~.,data=pisaTrain)
summary(lmScore)

#3.2
# Sum of Squared Errors
SSE = sum(lmScore$residuals^2)
# Root mean squared error
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

#4.1
# Make predictions on test set
PredTest = predict(lmScore, newdata=pisaTest)
range(PredTest)[2]-range(PredTest)[1]

#4.2
# Sum of Squared Errors
SSE = sum((PredTest-pisaTest$readingScore)^2)
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE

#4.3 baseline score
baseline<-mean(pisaTrain$readingScore)
baseline
SST=sum((pisaTest$readingScore-baseline)^2)
SST

#4.4 Rsq on test set
RSQ<-1-SSE/SST
RSQ