myDataFrame <- read.csv(file="f:/ipl/deliveries.csv", header=TRUE, sep=",")
SRHScore <- sum(subset(myDataFrame, match_id == 577 & inning == 1, select = c(total_runs)))
target <- SRHScore + 1
RCBRRToDraw <- SRHScore / 20

cat("SRH Scored : ", SRHScore, "\n")
cat("Target : ", target, "\n")
cat("Run Rate required by RCB to draw : ", RCBRRToDraw, "\n")

RCBRunsPerOver <- NULL

for (overNumber in 1:20)
{
	RCBRunsPerOver[overNumber] <- sum(subset(myDataFrame, match_id == 577 & inning == 2 & over == overNumber, select = c(total_runs)))
}

barplot(RCBRunsPerOver, names.arg=(1:20), main="Runs Per Over", xlab="Over", ylab="Runs")
cat("The bar graph is unimodal, with the highest number of runs scored in the middle of the game.\n")
