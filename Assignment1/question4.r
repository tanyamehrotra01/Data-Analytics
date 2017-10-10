myDataFrame <- read.csv(file="f:/ipl/deliveries.csv", header=TRUE, sep=",")

dhoniRuns <- NULL
kohliRuns <- NULL

dhoniCounter <- 0
kohliCounter <- 0

for (matchID in 1:577)
{	
	if (any("MS Dhoni" == subset(myDatasFrame, match_id == matchID, select = c(batsman))))
	{
		dhoniRuns[dhoniCounter] <- sum(subset(myDatasFrame, match_id == matchID & batsman == "MS Dhoni", select = c(batsman_runs)))
		dhoniCounter <- dhoniCounter + 1
	}

	if (any("V Kohli" == subset(myDatasFrame, match_id == matchID, select = c(batsman))))
	{
		kohliRuns[kohliCounter] <- sum(subset(myDatasFrame, match_id == matchID & batsman == "V Kohli", select = c(batsman_runs)))
		kohliCounter <- kohliCounter + 1
	}
}

boxplot(dhoniRuns,  main = "MS Dhoni Runs")
cat("Dhoni's average performance is about 23 runs. He has scored a maximum of 70 runs. Most of his runs are in the lower scores (20-40).")

boxplot(kohliRuns,  main = "V Kohli Runs")
cat("Kohli has a few outstanding performances that we can see as outliers in the boxplot.")

cat("Solely based on consistancy, I would choose Dhoni.")