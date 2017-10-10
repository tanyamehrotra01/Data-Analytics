#QUESTION2

library(caret)
library(corrplot)
library(plyr)

# Part 1

cancerFrame <- read.csv("D:/Data Analytics/Assignment 4/question2/cancer.csv", header = TRUE, sep = ",")
cancerFrame <- na.omit(cancerFrame)
initialColumnNames <- colnames(cancerFrame)
cat("\nDropping 'id' (of the patient) field because it has no effect on predicting the class of the cancer.\n")
cancerFrame <- cancerFrame[, -which(colnames(cancerFrame) %in% c("id"))]
numericData <- cancerFrame[sapply(cancerFrame, is.numeric)]
cat("\nApplying PCA on the remaining data.\n")
correlationMatrix <- cor(numericData)
corrplot(correlationMatrix, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))
cat("\nRemoving those fields which have a correlation of more than 0.7.\n")
highlyCorrelatedIndices <- findCorrelation(correlationMatrix, cutoff=0.7)
highlyCorrelatedColumns <- colnames(numericData)[highlyCorrelatedIndices]
newCancerFrame <- cancerFrame[, -which(colnames(cancerFrame) %in% highlyCorrelatedColumns)]
finalColumnNames <- colnames(newCancerFrame)
droppedColumnNames <- initialColumnNames[-which((finalColumnNames) %in% initialColumnNames)]

cat("\nDropped columns:\n", droppedColumnNames, "\n")
cat("\nFinal columns:\n", finalColumnNames, "\n")

# Part 2

cancerFrame <- read.csv("D:/Data Analytics/Assignment 4/question 2/cancer_tailored.csv", header = TRUE, sep = ",")
numberOfTruePositives <- nrow(subset(cancerFrame, (cancerFrame$diagnosis == 'M') & (cancerFrame$predicted == 'M')))
numberOfFalseNegatives <- nrow(subset(cancerFrame, (cancerFrame$diagnosis == 'M') & (cancerFrame$predicted == 'B')))
numberOfFalsePositives <- nrow(subset(cancerFrame, (cancerFrame$diagnosis == 'B') & (cancerFrame$predicted == 'M')))
numberOfTrueNegatives <- nrow(subset(cancerFrame, (cancerFrame$diagnosis == 'B') & (cancerFrame$predicted == 'B')))
confusionMatrix <- matrix(c(numberOfTruePositives, numberOfFalseNegatives, numberOfFalsePositives, numberOfTrueNegatives), nrow = 2, ncol = 2, byrow = TRUE)
dimnames(confusionMatrix) = list(c("A", "not A"), c("A", "not A"))
print(confusionMatrix)

# Part 3

accuracy <- (numberOfTruePositives + numberOfTrueNegatives) / nrow(cancerFrame)
misclassificationRate <- 1 - accuracy
recall <- numberOfTruePositives / (numberOfTruePositives + numberOfFalseNegatives)
precision <- numberOfTruePositives / (numberOfTruePositives + numberOfFalsePositives)
specificity <-numberOfTrueNegatives / (numberOfFalseNegatives + numberOfTruePositives)
fscore_two <- (1 + (2^2)) * ((precision * recall) / (((2^2) * precision) + recall))
fscore_half <- (1 + (0.5^0.5)) * ((precision * recall) / (((0.5^0.5) * precision) + recall)) 

cat("\nAccuracy: ", accuracy, "\n")
cat("Misclassification Rate: ", misclassificationRate, "\n")
cat("Recall: ", recall, "\n")
cat("Precision: ", precision, "\n")
cat("Specificity: ", specificity, "\n")
cat("F Score with B = 2: ", fscore_two, "\n")
cat("F Score with B = 0.5: ", fscore_half, "\n")
cat("\nThe Beta parameter determines the weight of precision in the combined score.\nBeta < 1 lends more weight to precision, while Beta > 1 favors recall\n")

#QUESTION3
#The slope in linear regression gives marginal change in the output varable by changing the independent variable by unit distance.
#Correlation has no slope. It is normalized covariance with the standard deviation of both the factors. the reason for doing the above is to get correlation
#within the range of -1 and 1. It is difficult to compare covariance since it depends upon the units of two variables.
#Therfore we need to normalize the covariance with some spread to ensure we compare like variables. This number after normalization is termed as 
#nothing but correlation.