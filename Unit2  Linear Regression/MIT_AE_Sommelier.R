# Set working directory
setwd("C:/Users/Mike/Rspace/MIT_AE/Unit2")

library(dplyr)
library(lubridate)

## Sommelier

if(!file.exists("data")){
        dir.create("data")
}

# download wine data if not yet already done so
if(!file.exists("./data/wine.csv")){
        
        #download a data file as csv into the "data" dir
        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/wine.csv"
        download.file(fileURL,destfile="./data/wine.csv")       
}

wine<-read.csv("./data/wine.csv",stringsAsFactors=FALSE)

# download wine test data if not yet already done so
if(!file.exists("./data/wine_test.csv")){
        
        #download a data file as csv into the "data" dir
        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/wine_test.csv"
        download.file(fileURL,destfile="./data/wine_test.csv")        
}

wineTest<-read.csv("./data/wine_test.csv",stringsAsFactors=FALSE)

QQ3
fit<-lm(Price~WinterRain+HarvestRain,data=wine)
summary(fit)

QQ5
cor(wine$HarvestRain,wine$WinterRain)
plot(wine$HarvestRain,wine$WinterRain)