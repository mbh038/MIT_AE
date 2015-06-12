
library(dplyr)
WHO<-read.csv("WHO.csv",stringsAsFactors=FALSE)
WHO_Europe<-filter(WHO,Region=="Europe")
str(WHO)
str(WHO_Europe)
write.csv(WHO_Europe,"WHO_Europe.csv")
