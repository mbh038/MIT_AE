## THE ANALYTICS EDGE 15.071x

## UNIT 7 A2

## Michael Hunt

## July 2015

# The cliche goes that the world is an increasingly interconnected place, and the
# connections between different entities are often best represented with a graph. 
# Graphs are comprised of vertices (also often called "nodes") and edges 
# connecting those nodes. In this assignment, we will learn how to visualize 
# networks using the igraph package in R.

# For this assignment, we will visualize social networking data using anonymized
# data from Facebook; this data was originally curated in a recent paper about
# computing social circles in social networks. In our visualizations, 
# the vertices in our network will represent Facebook users and the edges will 
# represent these users being Facebook friends with each other.

# The first file we will use, edges.csv, contains variables V1 and V2, which 
# label the endpoints of edges in our network. Each row represents a pair of
# users in our graph who are Facebook friends. For a pair of friends A and B,
# edges.csv will only contain a single row -- the smaller identifier will be 
# listed first in this row. From this row, we will know that A is friends with
# B and B is friends with A.

# The second file, users.csv, contains information about the Facebook users, who 
# are the vertices in our network. This file contains the following variables:
        
# id: A unique identifier for this user; this is the value that appears in the 
# rows of edges.csv

# gender: An identifier for the gender of a user taking the values A and B. 
# Because the data is anonymized, we don't know which value refers to males and which value refers to females.

# school: An identifier for the school the user attended taking the values A and
# AB (users with AB attended school A as well as another school B). 
# Because the data is anonymized, we don't know the schools represented by A and B.

# locale: An identifier for the locale of the user taking the values A and B. 
# Because the data is anonymized, we don't know which value refers to what locale.

## VISUALIZING NETWORK DATA
edges=read.csv("./data/edges.csv")
users=read.csv("./data/users.csv")

str(edges)
summary(edges)
head(edges)

str(users)
summary(users)
head(users)

# 1.1 How many Facebook users are there in our dataset?
str(users)

# 1.1 In our dataset, what is the average number of friends per user?
# =2e/u

2*nrow(edges)/nrow(users)

# 1.2 
# Out of all the students who listed a school, what was the most common locale?
table(users$locale,users$school) # All went to locale B

# 1.3 
# Is it possible that either school A or B is an all-girls or all-boys school?
table(users$gender,users$school) # No!

## CREATING A NETWORK

# 2.1
library(igraph)
?graph.data.frame

# 2.2
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

# 2.3
degree(g)
table(degree(g))

# 2.4
V(g)$size = degree(g)/2+2
V(g)
plot(g, vertex.label=NA)
max(V(g)$size)
min(V(g)$size)

## COLORING VERTICES

# 3.1 colour by gender
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

# 3.2 colour by school
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "yellow"
plot(g, vertex.label=NA)

# 3.3 colour by locale
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "yellow"
plot(g, vertex.label=NA)

## 4. OTHER PLOTTING OPTIONS
?igraph.plotting
