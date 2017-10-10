# Preprocessing of data

# a)

# Here the season and episode number is being converted into the progress made
# by that episode in the season when the episode has completed airing.

viewershipFrame <- read.csv("got_viewership.csv", header = TRUE, sep = ",")

progress <- NULL
progressCounter <- 1

for (i in c(min(subset(viewershipFrame, select = "Season")):max(subset(viewershipFrame, select = "Season"))))
{
	numberOfEpisodesInSeason <- nrow(subset(viewershipFrame, viewershipFrame$Season == i))

	for (j in c(min(subset(viewershipFrame, viewershipFrame$Season == i, select = "Epsisode")):max(subset(viewershipFrame, viewershipFrame$Season == i, select = "Epsisode"))))
	{
		progress[progressCounter] <- (i-1) + ((j)/numberOfEpisodesInSeason)
		progressCounter <- progressCounter + 1
	}
}

viewershipFrame$Season_Progress <- progress


# Building a model

# b)

# Multiple Linear Regression

Season_Progress <- viewershipFrame$Season_Progress
Year <- viewershipFrame$Year
Number_of_Major_Deaths <- viewershipFrame$Number.of.Major.Deaths
Critic_Ratings <- viewershipFrame$Critic.Ratings
Viewership_in_million <- viewershipFrame$Viewership..in.million

linearRegressionModel <- lm(Viewership_in_million ~ Season_Progress + Year + Number_of_Major_Deaths + Critic_Ratings)

# c)

linearRegressionModelWithoutCriticRatings <- lm(Viewership_in_million ~ Season_Progress + Year + Number_of_Major_Deaths)


# Prediction using models and visualization

# d)

# Using linear regression model without considering the critic ratings is a better option

numberOfMajorDeaths_Season7 <- mean(viewershipFrame$Number.of.Major.Deaths)
criticRatings_Season7 <- min(viewershipFrame$Critic.Ratings)

viewershipSeason7Frame <- read.csv("got_season7_ratings.csv", header = TRUE, sep = ",") 

season7Progress <- NULL
season7ProgressCounter <- 1

year <- NULL
numberOfMajorDeaths <- NULL
criticRatings <- NULL

numberOfEpisodesInSeason7 <- nrow(subset(viewershipSeason7Frame))

for (i in c(min(subset(viewershipSeason7Frame, select = "episode")):max(subset(viewershipSeason7Frame, select = "episode"))))
{
	season7Progress[season7ProgressCounter] <- 6 + ((i)/numberOfEpisodesInSeason7)
	year[season7ProgressCounter] <- 2017
	numberOfMajorDeaths[season7ProgressCounter] <- numberOfMajorDeaths_Season7
	criticRatings[season7ProgressCounter] <- criticRatings_Season7

	season7ProgressCounter <- season7ProgressCounter + 1
}

predictionFrame <- data.frame(Season_Progress = season7Progress, Year = year, Number_of_Major_Deaths = numberOfMajorDeaths, Critic_Ratings = criticRatings)
predictionFrameWithoutCriticRatings <- data.frame(Season_Progress = season7Progress, Year = year, Number_of_Major_Deaths = numberOfMajorDeaths)

predictionWithCriticRatings <- predict(linearRegressionModel, predictionFrame)
predictionWithoutCriticRatings <- predict(linearRegressionModelWithoutCriticRatings, predictionFrameWithoutCriticRatings)

actual <- subset(viewershipSeason7Frame, select = "ratings")

total <- 0
for (i in c(1:7))
{
	total <- total + ((predictionWithCriticRatings[i] - actual$ratings[i]) ** 2)	
}

mean <- total/7

rmsWithCriticRatings <- mean ** 0.5

total <- 0
for (i in c(1:7))
{
	total <- total + ((predictionWithoutCriticRatings[i] - actual$ratings[i]) ** 2)	
}

mean <- total/7

rmsWithoutCriticRatings <- mean ** 0.5

cat ("RMS With Critic Ratings :", rmsWithCriticRatings, "\n")
cat ("RMS Without Critic Ratings :", rmsWithoutCriticRatings, "\n")

# e)

par(mfrow=c(1,3))
plot(c(1:7), actual$ratings, type = "l")
plot(c(1:7), predictionWithCriticRatings, type = "l")
plot(c(1:7), predictionWithoutCriticRatings, type = "l")