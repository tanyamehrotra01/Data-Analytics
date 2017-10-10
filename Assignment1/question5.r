delmyDataFrame <- read.csv(file="f:/ipl/deliveries.csv", header=TRUE, sep=",")

rainaRuns <- NULL
rainaCounter <- 0

glRuns <- NULL
glCounter <- 0

glNotRuns <- NULL
glNotCounter <- 0

glGamesVerdict <- NULL
glGamesCounter <- 0

for (matchID in 1:577)
{	
	if (any("Gujarat Lions" == subset(myDataFrame, match_id == matchID, select = c(batting_team))))
	{
		if (any("SK Raina" == subset(myDataFrame, match_id == matchID, select = c(batsman))))
		{
			rainaRuns[rainaCounter] <- sum(subset(myDataFrame, match_id == matchID & batsman == "SK Raina", select = c(batsman_runs)))
			rainaCounter <- rainaCounter + 1
		}
		else
		{
			rainaRuns[rainaCounter] <- 0
			rainaCounter <- rainaCounter + 1	
		}

		currentGLRuns <- glRuns[glCounter] <- sum(subset(myDataFrame, match_id == matchID & batting_team == "Gujarat Lions", select = c(total_runs)))
		glCounter <- glCounter + 1

		currentGLNotRuns <- glNotRuns[glNotCounter] <- sum(subset(myDataFrame, match_id == matchID & batting_team != "Gujarat Lions", select = c(total_runs)))
		glNotCounter <- glNotCounter + 1

		if (currentGLNotRuns > currentGLRuns)
		{
		 	glGamesVerdict[glGamesCounter] <- "Lose"
		}
		else
		{
			glGamesVerdict[glGamesCounter] <- "Win"
		}

		glGamesCounter <- glGamesCounter + 1
	}
}

cat(glRuns, "\n")
cat(rainaRuns, "\n")
cat(glGamesVerdict, "\n\n")

cat("Raina has not played well for the Gujarat Lions. Therefore, his
performance had very little effect on the outcome of the matches. When he
scored the highest runs of 75, his team lost; whereas when he scored his
second highest of 58, his team won.\n")