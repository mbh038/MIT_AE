## THE ANALYTICS EDGE 15.071x

## UNIT 6 A2

## Michael Hunt

## July 2015

## MARKET SEGMENTATION FOR AIRLINES

# Balance = number of miles eligible for award travel
# QualMiles = number of miles qualifying for TopFlight status
# BonusMiles = number of miles earned from non-flight bonus transactions in the past 12 months
# BonusTrans = number of non-flight bonus transactions in the past 12 months
# FlightMiles = number of flight miles in the past 12 months
# FlightTrans = number of flight transactions in the past 12 months
# DaysSinceEnroll = number of days since enrolled in the frequent flyer program

# load data into R
airlines<-read.csv("./data/AirlinesCluster.csv")
str(airlines)
summary(airlines) # see that some variables orders of magnitude greater than others - need to normalise before clustering.

## NORMALISING THE DATA
#install.packages("caret")
library(caret)

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines) # x (in airlines) -> (x-mean)/st dev.
summary(airlinesNorm)

## HIERARCHICAL CLUSTERING

# compute euclidean distances
distances = dist(airlinesNorm, method = "euclidean")

# Hierarchical clustering
HierClusterAir = hclust(distances, method = "ward.D")

# Plot the dendrogram
plot(HierClusterAir)

# decide on 5 clusters

# Assign points to clusters
HierAirClusters = cutree(HierClusterAir, k = 5)

# how many points in cluster 1?
HierAirClusterList = split(airlinesNorm, HierAirClusters)
HierAir1<-HierAirClusterList[[1]]
nrow(HierAir1)

# Find mean values of each vaiable across the 5 clusters
tapply(airlines$Balance, HierAirClusters, mean)
tapply(airlines$QualMiles, HierAirClusters, mean)
tapply(airlines$BonusMiles, HierAirClusters, mean)
tapply(airlines$BonusTrans, HierAirClusters, mean)
tapply(airlines$FlightMiles, HierAirClusters, mean)
tapply(airlines$FlightTrans, HierAirClusters, mean)
tapply(airlines$DaysSinceEnroll, HierAirClusters, mean)

## K-MEANS CLUSTERING
# Specify number of clusters
k = 5
# Run k-means
set.seed(88)
KMCAir = kmeans(airlinesNorm, centers = k,iter.max=1000)
str(KMCAir)
table(KMCAir$cluster)

# compare the the two clustering methods
table(HierAirClusters,KMCAir$cluster) # gives Hier values in rows, k-means values in columns