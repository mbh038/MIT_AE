## KAGGLE COMPETITION




sample<-read.csv("./data/SampleSubmission.csv")
head(sample)

# How to make a submission

# Supposing that...

# test set probability predictions are called "testPred"
# test data set is "test"

submission = data.frame(UniqueID = test$UniqueID, Probability1 = testPred)
write.csv(submission, "submission.csv", row.names=FALSE)

