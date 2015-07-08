## THE ANALYTICS EDGE 15.071x

## UNIT 6 A3

## Michael Hunt

## July 2015

## PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT

# ReturnJan = the return for the company's stock during January (in the year of the observation). 
# ReturnFeb = the return for the company's stock during February (in the year of the observation). 
# ReturnMar = the return for the company's stock during March (in the year of the observation). 
# ReturnApr = the return for the company's stock during April (in the year of the observation). 
# ReturnMay = the return for the company's stock during May (in the year of the observation). 
# ReturnJune = the return for the company's stock during June (in the year of the observation). 
# ReturnJuly = the return for the company's stock during July (in the year of the observation). 
# ReturnAug = the return for the company's stock during August (in the year of the observation). 
# ReturnSep = the return for the company's stock during September (in the year of the observation). 
# ReturnOct = the return for the company's stock during October (in the year of the observation). 
# ReturnNov = the return for the company's stock during November (in the year of the observation). 
# PositiveDec = whether or not the company's stock had a positive return in December (in the year of the observation).
#   This variable takes value 1 if the return was positive, and value 0 if the return was not positive.

# For the first 11 variables, the value stored is a proportional change in stock value during that month.
# For instance, a value of 0.05 means the stock increased in value 5% during the month, while a value 
# of -0.02 means the stock decreased in value 2% during the month.

# load data into R
stocks<-read.csv("./data/StocksCluster.csv")
str(stocks)
summary(stocks)

# proportion of stocks giving positive returns in December
mean(stocks$PositiveDec)

# pairwise correlations between all the Return variables
round(cor(stocks[,1:11]),3)

library(Matrix)
max(cor(stocks[,1:11])-Diagonal(11))
