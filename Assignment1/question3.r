Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

myDataFrame <- read.csv(file="/home/varun/Desktop/Data Analytics/Assignment 1/deliveries.csv", header=TRUE, sep=",")

devilliersRuns <- NULL
dhoniRuns <- NULL
jadejaRuns <- NULL
rainaRuns <- NULL
kohliRuns <- NULL


devilliersCounter <- 0
dhoniCounter <- 0
jadejaCounter <- 0
rainaCounter <- 0
kohliCounter <- 0

for (matchID in 1:577)
{	
	if (any("AB de Villiers" == subset(myDataFrame, match_id == matchID, select = c(batsman))))
	{
		devilliersRuns[devilliersCounter] <- sum(subset(myDataFrame, match_id == matchID & batsman == "AB de Villiers", select = c(batsman_runs)))
		devilliersCounter <- devilliersCounter + 1
	}

	if (any("MS Dhoni" == subset(myDataFrame, match_id == matchID, select = c(batsman))))
	{
		dhoniRuns[dhoniCounter] <- sum(subset(myDataFrame, match_id == matchID & batsman == "MS Dhoni", select = c(batsman_runs)))
		dhoniCounter <- dhoniCounter + 1
	}

	if (any("RA Jadeja" == subset(myDataFrame, match_id == matchID, select = c(batsman))))
	{
		jadejaRuns[jadejaCounter] <- sum(subset(myDataFrame, match_id == matchID & batsman == "RA Jadeja", select = c(batsman_runs)))
		jadejaCounter <- jadejaCounter + 1
	}

	if (any("SK Raina" == subset(myDataFrame, match_id == matchID, select = c(batsman))))
	{
		rainaRuns[rainaCounter] <- sum(subset(myDataFrame, match_id == matchID & batsman == "SK Raina", select = c(batsman_runs)))
		rainaCounter <- rainaCounter + 1
	}

	if (any("V Kohli" == subset(myDataFrame, match_id == matchID, select = c(batsman))))
	{
		kohliRuns[kohliCounter] <- sum(subset(myDataFrame, match_id == matchID & batsman == "V Kohli", select = c(batsman_runs)))
		kohliCounter <- kohliCounter + 1
	}
}

cat("AB de Villiers\n")
cat("\tMean : ", mean(devilliersRuns), "\n")
cat("\tMedian : ", median(devilliersRuns), "\n")
cat("\tMode : ", Mode(devilliersRuns), "\n")
cat("\tQuartile 1 : ", quantile(devilliersRuns, 0.25), "\n")
cat("\tQuartile 3 : ", quantile(devilliersRuns, 0.75), "\n")
cat("\tIQR : ", quantile(devilliersRuns, 0.75) - quantile(devilliersRuns, 0.25), "\n")
cat("\tStandard Deviation : ", sd(devilliersRuns), "\n\n")

cat("MS Dhoni\n")
cat("\tMean : ", mean(dhoniRuns), "\n")
cat("\tMedian : ", median(dhoniRuns), "\n")
cat("\tMode : ", Mode(dhoniRuns), "\n")
cat("\tQuartile 1 : ", quantile(dhoniRuns, 0.25), "\n")
cat("\tQuartile 3 : ", quantile(dhoniRuns, 0.75), "\n")
cat("\tIQR : ", quantile(dhoniRuns, 0.75) - quantile(dhoniRuns, 0.25), "\n")
cat("\tStandard Deviation : ", sd(dhoniRuns), "\n\n")

cat("RA Jadeja\n")
cat("\tMean : ", mean(jadejaRuns), "\n")
cat("\tMedian : ", median(jadejaRuns), "\n")
cat("\tMode : ", Mode(jadejaRuns), "\n")
cat("\tQuartile 1 : ", quantile(jadejaRuns, 0.25), "\n")
cat("\tQuartile 3 : ", quantile(jadejaRuns, 0.75), "\n")
cat("\tIQR : ", quantile(jadejaRuns, 0.75) - quantile(jadejaRuns, 0.25), "\n")
cat("\tStandard Deviation : ", sd(jadejaRuns), "\n\n")

cat("SK Raina\n")
cat("\tMean : ", mean(rainaRuns), "\n")
cat("\tMedian : ", median(rainaRuns), "\n")
cat("\tMode : ", Mode(rainaRuns), "\n")
cat("\tQuartile 1 : ", quantile(rainaRuns, 0.25), "\n")
cat("\tQuartile 3 : ", quantile(rainaRuns, 0.75), "\n")
cat("\tIQR : ", quantile(rainaRuns, 0.75) - quantile(rainaRuns, 0.25), "\n")
cat("\tStandard Deviation : ", sd(rainaRuns), "\n\n")

cat("V Kohli\n")
cat("\tMean : ", mean(kohliRuns), "\n")
cat("\tMedian : ", median(kohliRuns), "\n")
cat("\tMode : ", Mode(kohliRuns), "\n")
cat("\tQuartile 1 : ", quantile(kohliRuns, 0.25), "\n")
cat("\tQuartile 3 : ", quantile(kohliRuns, 0.75), "\n")
cat("\tIQR : ", quantile(kohliRuns, 0.75) - quantile(kohliRuns, 0.25), "\n")
cat("\tStandard Deviation : ", sd(kohliRuns), "\n\n")