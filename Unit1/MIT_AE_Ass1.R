# Set working directory
setwd("C:/Users/Mike/Rspace/MIT_AE/Unit1")

library(dplyr)
library(lubridate)

## AN ANALYTICAL DETECTIVE

if(!file.exists("data")){
        dir.create("data")
}

# download data if not yet already done so
if(!file.exists("./data/mvtWeek1.csv")){
        
        #download a data file as csv into the "data" dir
        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/mvtWeek1.csv"
        download.file(fileURL,destfile="./data/mvtWeek1.csv")
        #include date of download
        
}

mvt<-read.csv("./data/mvtWeek1.csv",stringsAsFactors=FALSE)

#1.1
nrow(mvt)


#1.2
str(mvt)

#1.3
max(mvt$ID)

#1.4
min(mvt$Beat)

#1.5
sum(mvt$Arrest==TRUE)

#1.6
sum(mvt$LocationDescription=="ALLEY")

#1.7
mvt$month<-as.POSIXlt(mdy_hm(mvt$Date))$mon+1

gtamax<-mvt %>%
        filter(Arrest==TRUE) %>%
        group_by(month) %>%
        count(month) %>%
        arrange(-n)
gtamax

#2.2
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
median(DateConvert)

#2.3
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

sort(table(mvt$Month))

#2.4
sort(table(mvt$Weekday))

#2.5
hist(mvt$Date, breaks=100)

#3.2
boxplot(Date~Arrest,mvt)

#3.3
year<-2001
sum(mvt$Arrest==TRUE & mvt$Year == year)/
        table(mvt$Year)[match(year,names(table(mvt$Year)))]


#3.4
year<-2007
sum(mvt$Arrest==TRUE & mvt$Year == year)/
        table(mvt$Year)[match(year,names(table(mvt$Year)))]

#3.5
year<-2012
sum(mvt$Arrest==TRUE & mvt$Year == year)/
        table(mvt$Year)[match(year,names(table(mvt$Year)))]

#4.1
tail(sort(table(mvt$LocationDescription)))

#4.2
Top5<-subset(mvt,LocationDescription == "STREET" |
                     LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
                     LocationDescription == "ALLEY" |
                     LocationDescription == "GAS STATION" |
                     LocationDescription == "DRIVEWAY - RESIDENTIAL")
nrow(Top5)

#4.3
table(Top5$LocationDescription,Top5$Arrest==TRUE)[,2]/
        (table(Top5$LocationDescription,Top5$Arrest==TRUE)[,1]
         +table(Top5$LocationDescription,Top5$Arrest==TRUE)[,2])

#4.4
worstdays<-Top5 %>%
        filter(LocationDescription=="GAS STATION") %>%
        group_by(Weekday) %>%
        count(Weekday) %>%
        arrange(-n)
worstdays

#4.5
bestdays<-Top5 %>%
        filter(LocationDescription=="DRIVEWAY - RESIDENTIAL") %>%
        group_by(Weekday) %>%
        count(Weekday) %>%
        arrange(n)
bestdays

## STOCK DYNAMICS

# download data if not yet already done so, then load into R

#IBM
if(!file.exists("./data/IBMStock.csv")){        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/IBMStock.csv"
        download.file(fileURL,destfile="./data/IBMStock.csv")
}
IBM<-read.csv("./data/IBMStock.csv",stringsAsFactors=FALSE)

# GE
if(!file.exists("./data/GEStock.csv")){        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/GEStock.csv"
        download.file(fileURL,destfile="./data/GEStock.csv")
}
GE<-read.csv("./data/GEStock.csv",stringsAsFactors=FALSE)

# ProcterGamble
if(!file.exists("./data/ProcterGambleStock.csv")){        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/ProcterGambleStock.csv"
        download.file(fileURL,destfile="./data/ProcterGambleStock.csv")
}
ProcterGamble<-read.csv("./data/ProcterGambleStock.csv",stringsAsFactors=FALSE)

# Coca Cola
if(!file.exists("./data/CocaColaStock.csv")){        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/CocaColaStock.csv"
        download.file(fileURL,destfile="./data/CocaColaStock.csv")
}
CocaCola<-read.csv("./data/CocaColaStock.csv",stringsAsFactors=FALSE)

# Boeing
if(!file.exists("./data/BoeingStock.csv")){        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/BoeingStock.csv"
        download.file(fileURL,destfile="./data/BoeingStock.csv")
}
Boeing<-read.csv("./data/BoeingStock.csv",stringsAsFactors=FALSE)

#1.1
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#1.2
IBM[1,]
GE[1,]

#1.3
IBM[nrow(IBM),]
GE[nrow(GE),]

#1.4
mean(IBM$StockPrice)

#1.5
min(GE$StockPrice)

#1.6
max(CocaCola$StockPrice)

#1.7
median(Boeing$StockPrice)

#1.7
sd(ProcterGamble$StockPrice)

#2.1
plot(x=CocaCola$Date,y=CocaCola$StockPrice,"l",xlab="Date",ylab="Stock Price",col="red")

#2.2
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

#2.3
abline(v=as.Date(c("1983-06-01")), lwd=2)

#3.1
plot(CocaCola$Date[301:432], 
     CocaCola$StockPrice[301:432],
     type="l", col="red", ylim=c(0,210))

lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432],col="green")
lines(IBM$Date[301:432], IBM$StockPrice[301:432],col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],col="black")

abline(v=as.Date(c("2000-03-01")), lwd=2)

#3.3
abline(v=as.Date(c("1997-09-01")), lwd=1)
abline(v=as.Date(c("1997-11-01")), lwd=1)

#3.4
abline(v=as.Date(c("2004-01-01")), lwd=1)

#4.1
print("IBM")
sort(tapply(IBM$StockPrice,months(IBM$Date),mean)/mean(IBM$StockPrice))


#4.2
print("GE")
sort(tapply(GE$StockPrice,months(GE$Date),mean)/mean(GE$StockPrice))
print("ProcterGamble")
sort(tapply(ProcterGamble$StockPrice,months(ProcterGamble$Date),mean)/mean(ProcterGamble$StockPrice))
print("Boeing")
sort(tapply(Boeing$StockPrice,months(Boeing$Date),mean)/mean(Boeing$StockPrice))
print("CocaCola")
sort(tapply(CocaCola$StockPrice,months(CocaCola$Date),mean)/mean(CocaCola$StockPrice))

## DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES 

# Download data

if(!file.exists("data")){
        dir.create("data")
}

# download data if not yet already done so
if(!file.exists("./data/CPS.csv")){
        
        #download a data file as csv into the "data" dir
        
        fileURL<-"https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/CPSData.csv"
        download.file(fileURL,destfile="./data/CPS.csv")
 
     
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

#3.2
CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
sum(is.na(CPS$MetroAreaCode))

#3.3
n.interviewee<-
        CPS %>%
        group_by(MetroArea) %>% 
        count(MetroArea) %>%
        arrange(-n)
n.interviewee

#3.4
n.hispanic<-
        CPS %>%
        group_by(MetroArea) %>% 
        summarise(hisp=sum(Hispanic))
n.hispanic        

n.hisp.int<-merge(n.interviewee,n.hispanic,by.x="MetroArea",all.x=TRUE)
n.hisp.int$hispfrac<-n.hisp.int$hisp/n.hisp.int$n
arrange(n.hisp.int,-hispfrac)[1,1]

#3.5
n.asian<-
        CPS %>%
        group_by(MetroArea) %>% 
        summarise(asians=sum(Race=="Asian"))
n.asian        

n.asian.int<-merge(n.interviewee,n.asian,by.x="MetroArea",all.x=TRUE)
n.asian.int$asianfrac<-n.asian.int$asian/n.asian.int$n
topasians<-count(n.asian.int,asianfrac>=0.2)[2,2]
topasians
arrange(n.asian.int,-asianfrac)[1:5,]

#3.6
no_hs_dip<-sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))
head(no_hs_dip)

#4.1
CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

n.cob<-CPS %>%
        group_by(Country) %>%
        count(Country) %>%
        arrange(-n)
n.cob

#4.3
narea<-CPS %>%
        filter(MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA") %>%
        count(MetroArea)
narea[1,2]
nnotUS<-narea<-CPS %>%
        filter(MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA"
               & Country !="United States") %>%
        count(MetroArea)
nnotUS[1,2]

as.numeric(nnotUS[1,2])/as.numeric(narea[1,2]) # does not work!

#4.4
nindia<-CPS %>%
        group_by(MetroArea) %>%
        summarise(indians=sum(Country=="India",na.rm=TRUE)) %>%
        arrange(-indians)
nindia

nbrazil<-CPS %>%
        group_by(MetroArea) %>%
        summarise(brazils=sum(Country=="Brazil",na.rm=TRUE)) %>%
        arrange(-brazils)
nbrazil

nsomalia<-CPS %>%
        group_by(MetroArea) %>%
        summarise(somalians=sum(Country=="Somalia",na.rm=TRUE)) %>%
        arrange(-somalians)
nsomalia
