Unit3A2.R

## POPULARITY OF MUSIC RECORDS

## UNDERSTANDING THE DATA

songs<-read.csv("./data/songs.csv")
str(songs)
summary(songs)
#head(songs)

#1.1 How many observations (songs) are from the year 2010?
songs2010<-subset(songs,songs$year==2010)
nrow(songs2010)
library(dplyr)
songs2010<-filter(songs,year==2010)
nrow(songs2010)

#1.2 How many songs does the dataset include for which the artist name is "Michael Jackson"?
nrow(filter(songs,artistname=="Michael Jackson"))

#1.3 Which of these songs by Michael Jackson made it to the Top 10? Select all that apply.
MJTop10<-filter(songs,artistname=="Michael Jackson" & Top10==1)
nrow(MJTop10)
MJTop10$songtitle[1]
MJTop10$songtitle[2]
MJTop10$songtitle[3]
MJTop10$songtitle[4]
MJTop10$songtitle[5]

#1.4
#The variable corresponding to the estimated time signature (timesignature) is discrete, meaning
#that it only takes integer values (0, 1, 2, 3, . . . ). What are the values of this variable that
#occur in our dataset? Select all that apply.
unique(songs$timesignature)

#Which timesignature value is the most frequent among songs in our dataset?
table(songs$timesignature)

#1.5 
#Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs.
#Which one is it?
top_n(songs, 1, tempo)$songtitle

##CREATING OUR PREDICTION MODEL

#2.1
SongsTrain<-filter(songs,year<2010)
nrow(songsTrain)
SongsTest<-filter(songs,year==2010)
nrow(songsTest)

#2.2
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

model1=glm(Top10~.,data=SongsTrain,family="binomial")
summary(model1)

##BEWARE OF MULTICOLLINEARITY ISSUES!

#3.1 - check for correlation between loudness and energy
cor(SongsTrain$loudness,SongsTrain$energy)

#3.2 - remove loudness 
cor(SongsTrain$loudness,SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

#3.2 - remove loudness 
cor(SongsTrain$loudness,SongsTrain$energy)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

##VALIDATING OUR MODEL

#4.1 accuracy
predictTest = predict(SongsLog3, type="response", newdata=SongsTest)
max(predictTest)

ct<-table(SongsTest$Top10, predictTest > 0.45)
ct

#specificity
ct[1,1]/(ct[1,1]+ct[1,2])

#sensitivity
ct[2,2]/(ct[2,1]+ct[2,2])

#accuracy
(ct[1,1]+ct[2,2])/sum(ct)

#4.2 baseline accuracy - predict no song gets to the Top10
(ct[1,1]+ct[1,2])/sum(ct)