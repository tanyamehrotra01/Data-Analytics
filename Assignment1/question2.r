myDataFrame <- read.csv(file = "f:/ipl/deliveries.csv")
teams <- unique(subset(myDataFrame, select = c("match_id","inning","batting_team")))

runs <- aggregate(total_runs ~ inning + match_id, data = myDataFrame, FUN = sum)
result <- merge(teams, runs)
finalResult <- result[order(result$match_id),]

cat("Total Runs:\n")
print(subset(finalResult, match_id == 7 | match_id == 2 | match_id == 67 | match_id == 171 | match_id == 414), row.names=FALSE)

matchesFrame <- read.csv(file = "f:/ipl/matches.csv")
matches <- subset(matchesFrame, select = c("id","result","dl_applied"))
allMatches <- subset(merge(finalResult, matches),(result == "normal" | result == "tie") & inning < 3 & dl_applied == 0)
maxRuns <- allMatches[which.max(allMatches$total_runs),]
minRuns <- allMatches[which.min(allMatches$total_runs),]

cat("\nMax Runs:\n")
print(subset(maxRuns, select = c("batting_team", "total_runs")), row.names=FALSE)
cat("\n")
cat("Min Runs:\n")
print(subset(minRuns, select = c("batting_team", "total_runs")), row.names=FALSE)