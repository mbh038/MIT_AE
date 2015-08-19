## The Analytics Edge

##  MITx 15.071x

## Michael Hunt
## August 2015

## Exam Question Three

## UNDERSTANDING RETAIL CONSUMERS

## PROBLEM 1 - READING IN THE DATA
households<-read.csv("./data/Households.csv")
str(households)

# n households that log transactions only in the morning
nrow(subset(households,MorningPct==100))

# n households that log transactions only in the afternoon
nrow(subset(households,AfternoonPct==100))

## PROBLEM 2 - DESCRIPTIVE STATISTICS
over150<-subset(households,households$AvgSalesValue >150)
min(over150$AvgDiscount)

Discountover25<-subset(households,households$AvgDiscount>25)
min(Discountover25$AvgSalesValue)

nrow(subset(households,households$NumVisits>=300))/nrow(households)

## PROBLEM 3 - IMPORTANCE OF NORMALIZING
summary(households)

## PROBLEM 4 - NORMALIZING THE DATA
library(caret)
preproc = preProcess(households)
householdsNorm = predict(preproc, households)
summary(householdsNorm)

## PROBLEM 5 - INTERPRETING THE DENDROGRAM  

# create a dendrogram
set.seed(1200)
distances <- dist(householdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

# see that 2,3 or 5 clusters are all suitable numbers.

## PROBLEM 6 - K-MEANS CLUSTERING

# make houshold matrix
hhmatrix<-as.matrix(householdsNorm)
hhvector<-as.vector(hhmatrix)

# Specify number of clusters
k = 10

# Run k-means
set.seed(200)
KMC = kmeans(householdsNorm, centers = k, iter.max = 1000)
summary(KMC$size)
table(KMC$size)

## PROBLEM 7 - UNDERSTANDING THE CLUSTERS
hhCluster = split(householdsNorm, KMC$cluster)

hh1<-hhCluster[[1]]
hh2<-hhCluster[[2]]
hh3<-hhCluster[[3]]
hh4<-hhCluster[[4]]
hh5<-hhCluster[[5]]
hh6<-hhCluster[[6]]
hh7<-hhCluster[[7]]
hh8<-hhCluster[[8]]
hh9<-hhCluster[[9]]
hh10<-hhCluster[[10]]
# all this is unnnecessary - just use KMC$centers

## PROBLEM 7,8 and 9 - UNDERSTANDING THE CLUSTERS
# KMC$centers shows centroids (mean values) for each variable for
# each cluster.
KMC$centers
# or could do tapply on each cvariable as per example below
# KMC$clusters is much easier!
tapply(householdsNorm$NumVisits, KMC$cluster, mean)

## PROBLEM 12 - INCREASING THE NUMBER OF CLUSTERS
# now try with just 5 clusters
# Specify number of clusters
k = 5
# Run k-means
set.seed(5000)
KMC = kmeans(householdsNorm, centers = k, iter.max = 1000)
summary(KMC$size)
table(KMC$size)

## PROBLEM 13 - DESCRIBING THE CLUSTERS
KMC$centers

## PROBLEM 14 - UNDERSTANDING CENTROIDS

## PROBLEM 15 - USING A VISUALIZATION
to showdistribution of number of visits within each cluster:
        
householdsNorm$cluster<-KMC$cluster
householdsNorm$cluster<-as.factor(householdsNorm$cluster)

# boxplot - yes
boxplot(NumVisits~cluster,data=householdsNorm)
# or - yes
ggplot(householdsNorm, aes(x=cluster, y=NumVisits)) + geom_boxplot()

# ggplot histogram - no
ggplot(data=householdsNorm,aes(x=cluster,fill=NumVisits))+geom_histogram(binwidth=.5, position="dodge")

# ggplot point - yes
ggplot(data=householdsNorm,aes(x=NumVisits,y=cluster))+geom_point()
