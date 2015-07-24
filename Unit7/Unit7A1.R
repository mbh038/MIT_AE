## THE ANALYTICS EDGE 15.071x

## UNIT 7 A1

## Michael Hunt

## July 2015

## ELECTION FORECASTING REVISITED

# load packages
library(ggplot2)
library(maps)
library(ggmap)

# Load the map of the US
statesMap = map_data("state")

## 1. DRAWING A MAP OF THE US

# 1.1 How many groups are there?
str(statesMap)
length(unique(statesMap$group))

# 1.2
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

## 2. COLORING THE STATES BY PREDICTIONS

# read in data
polling=read.csv("./data/PollingImputed.csv")
str(polling)

# split data into Train (up to 2008) and Test sets
Train=subset(polling,Year<=2008 & Year >= 2004)
Test=subset(polling,Year>=2012)

# create a logistic regression model        
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

# make predictions on the test set
TestPrediction = predict(mod2, newdata=Test, type="response")

# TestPrediction gives the predicted probabilities for each state

# create a vector of Republican/Democrat predictions
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

# put the predictions and state labels in a data.frame so that we can use ggplot:
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

# How many states are predicted to be Republican for 2012?
nrow(predictionDataFrame[TestPredictionBinary==1,])

# average predicted probability for 2012
mean(predictionDataFrame$TestPrediction)

# 2.2 
# Now, we need to merge "predictionDataFrame" with the map data "statesMap",
# like we did in lecture. Before doing so, we need to convert the Test.State
# variable to lowercase, so that it matches the region variable in statesMap.
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

# Now, merge the two data frames
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

# Lastly, we need to make sure the observations are in order so that the map
# is drawn properly:
predictionMap = predictionMap[order(predictionMap$order),]

str(predictionMap)
str(statesMap)

# 2.4
# color the states according to our binary predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+
        geom_polygon(color = "black")

# 2.5
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+
        geom_polygon(color = "black") +
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# plot the probabilities instead of the binary predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+
        geom_polygon(color = "black") +
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

## 3.UNDERSTANDING THE PREDICTIONS

# 3.2
# What was our predicted probability for the state of Florida?
predictionDataFrame # look at row containing Florida

## 4. PARAMETER SETTINGS

