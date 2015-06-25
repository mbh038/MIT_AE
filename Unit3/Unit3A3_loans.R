Unit3A3.R

## PREDICTING REPAYMENT OF LOANS

## UNDERSTANDING THE DATA

loans<-read.csv("./data/loans.csv")
str(loans)
summary(loans)
#head(songs)

## PREPARING THE DATASET

#1.1
sum(loans$not.fully.paid)/nrow(loans)