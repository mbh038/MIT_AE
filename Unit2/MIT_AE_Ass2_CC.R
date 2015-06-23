# Set working directory
setwd("C:/Users/Mike/Rspace/MIT_AE/Unit2")

library(dplyr)
library(lubridate)

## CLIMATE CHANGE

if(!file.exists("data")){
        dir.create("data")
}

# download data if not yet already done so
if(!file.exists("./data/climate_change.csv")){
        
        #download a data file as csv into the "data" dir
        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/climate_change.csv"
        download.file(fileURL,destfile="./data/climate_change.csv")
        #include date of download
        
}

cc<-read.csv("./data/climate_change.csv",stringsAsFactors=FALSE)

#Problem 1.1

#training set
trn<-filter(cc,Year<2007)
#testing set
tst<-filter(cc,Year>=2007)

Next, build a linear regression model to predict the dependent variable
Temp, using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols
as independent variables (Year and Month should NOT be used in the model).
Use the training set to build the model.

model1<-lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data=trn)
summary(model1)

#Problem 1.2
cor(trn)

#Problem 3
#Given that the correlations are so high, let us focus on the N2O variable
#and build a model with only MEI, TSI, Aerosols and N2O as independent
# variables. Remember to use the training set to build the model.
model2<-lm(Temp~MEI+TSI+Aerosols+N2O,data=trn)
summary(model2)

#Problem 4
step1<-step(model1)
summary(step1)

#Problem 5
#Using the model produced from the step function, calculate temperature
#predictions for the testing data set, using the predict function.
Predictions = predict(step1, newdata=tst)
SSE = sum((Predictions - tst$Temp)^2)
SST = sum((mean(trn$Temp) - tst$Temp)^2)
R2 = 1 - SSE/SST
R2