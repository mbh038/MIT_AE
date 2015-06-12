# Set working directory
setwd("C:/Users/Mike/Rspace/MIT_AE")

library(dplyr)

# Download data

if(!file.exists("data")){
        dir.create("data")
}

# download data if not yet already done so
if(!file.exists("./data/CPS.csv")){
        
        #download a data file as csv into the "data" dir
        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/CPSData.csv"
        download.file(fileURL,destfile="./data/CPS.csv")
        #include date of download
     
}

CPS<-read.csv("./data/CPS.csv",stringsAsFactors=FALSE)


#view the data
summary(CPS)
str(CPS)

#1.1
nrow(CPS)

#1.2
a1.2<-sort(table(CPS$Industry))
a1.2
which.max(a1.2)

#1.3
which.min(sort(table(CPS$State)))
which.max(sort(table(CPS$State)))

#1.4
cnus<-nrow(filter(CPS,Citizenship=="Non-Citizen"))/nrow(CPS)
1-cnus

#1.5
CPS %>% group_by(Race) %>% summarise(hisp=sum(Hispanic)) %>%filter(hisp>250)
#or
table(CPS$Race, CPS$Hispanic) # seems more straightforward

#2.1
sapply(CPS,function(x){sum(is.na(x))>0})

#2.2
table(CPS$Region, is.na(CPS$Married))
                        
#2.3
nrow(table(CPS$State, mean(CPS$MetroAreaCode)==0))

#3.1
# download data if not yet already done so
if(!file.exists("./data/MetroAreaCodes.csv")){
        
        #download a data file as csv into the "data" dir
        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/MetroAreaCodes.csv"
        download.file(fileURL,destfile="./data/MetroAreaCodes.csv")
        #include date of download
        
}
MetroAreaMap<-read.csv("./data/MetroAreaCodes.csv",stringsAsFactors=FALSE)

if(!file.exists("./data/CountryCodes.csv")){
        
        #download a data file as csv into the "data" dir
        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/CountryCodes.csv"
        download.file(fileURL,destfile="./data/CountryCodes.csv")
        #include date of download
        
}
CountryMap<-read.csv("./data/CountryCodes.csv")

