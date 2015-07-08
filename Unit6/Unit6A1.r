## THE ANALYTICS EDGE 15.071x

## UNIT 6 A1

## Michael Hunt

## July 2015

# load data into R
dailykos<-read.csv("./data/dailykos.csv")
str(dailykos)

## Hierarchical clustering

# compute distances
distances = dist(dailykos, method = "euclidean")

# Hierarchical clustering
HierclusterKos = hclust(distances, method = "ward.D")

# Plot the dendrogram
plot(HierclusterKos)

# from Dendrogram, and from considration of what we want to do, 7 clusters seems a good idea.
# from Dendrogram alone, might ave gone for 2 or 3 clusters , but this would be too few to be
# useful.

# Assign points to clusters
Hierkosclusters = cutree(HierclusterKos, k = 7)

# create a data set for each cluster
table(Hierkosclusters)
Hierkos1<-subset(dailykos,Hierkosclusters==1)
Hierkos2<-subset(dailykos,Hierkosclusters==2)
Hierkos3<-subset(dailykos,Hierkosclusters==3)
Hierkos4<-subset(dailykos,Hierkosclusters==4)
Hierkos5<-subset(dailykos,Hierkosclusters==5)
Hierkos6<-subset(dailykos,Hierkosclusters==6)
Hierkos7<-subset(dailykos,Hierkosclusters==7)
nrow(Hierkos3) # should agreee with number itable check just above = 374

# could also do this using split function:
HierkosCluster = split(dailykos, Hierkosclusters)
Hierkos1<-HierkosCluster[[1]]
nrow(Hierkos1)
# etc...

# pick the six most frequently occurring words in each cluster
tail(sort(colMeans(kos1)))
tail(sort(colMeans(kos2)))
tail(sort(colMeans(kos3)))
tail(sort(colMeans(kos4)))
tail(sort(colMeans(kos5)))
tail(sort(colMeans(kos6)))
tail(sort(colMeans(kos7)))

## k-means clustering

# Specify number of clusters
k = 7
# Run k-means
set.seed(1000)
KMC = kmeans(dailykos, centers = k)
str(KMC)
table(KMC$cluster)

# split data set into the 7 clusters this produces
# lets try to do it using the split function this time:
kkosCluster = split(dailykos, KMC$cluster)
kkos1<-kkosCluster[[1]]
kkos2<-kkosCluster[[2]]
kkos3<-kkosCluster[[3]]
kkos4<-kkosCluster[[4]]
kkos5<-kkosCluster[[5]]
kkos6<-kkosCluster[[6]]
kkos7<-kkosCluster[[7]]

# find six most frequent words in each cluster
tail(sort(colMeans(kkos1)))
tail(sort(colMeans(kkos2)))
tail(sort(colMeans(kkos3)))
tail(sort(colMeans(kkos4)))
tail(sort(colMeans(kkos5)))
tail(sort(colMeans(kkos6)))
tail(sort(colMeans(kkos7)))

# compare the the two clustering methods
table(Hierkosclusters,KMC$cluster) # gives Hier values in rows, k-means values in columns
