flightsFrame <- read.csv("D:/Data Analytics/Assingment 3/question 2/flights.csv", header = TRUE, sep = ",")

# dates <- []

UA <- NULL
AA <- NULL
US <- NULL
F9 <- NULL
B6 <- NULL
OO <- NULL
AS <- NULL
NK <- NULL
WN <- NULL
DL <- NULL
EV <- NULL
HA <- NULL
MQ <- NULL
VX <- NULL
i <- 1

#considering the below selected parameters to decide upon the effiency of the flights
for (y in min(unique(subset(flightsFrame, select = c(YEAR)))):max(unique(subset(flightsFrame, select = c(YEAR)))))
{
	for (m in min(unique(subset(flightsFrame, flightsFrame$YEAR == y, select = c(MONTH)))):max(unique(subset(flightsFrame, flightsFrame$YEAR == y, select = c(MONTH)))))
	{
		for (d in min(unique(subset(flightsFrame, (flightsFrame$MONTH == m) & (flightsFrame$YEAR == y), select = c(DAY)))):max(unique(subset(flightsFrame, (flightsFrame$MONTH == m) & (flightsFrame$YEAR == y), select = c(DAY)))))
		{
			UA[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "UA") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			AA[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "AA") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			US[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "US") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			F9[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "F9") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			B6[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "B6") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			OO[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "OO") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			AS[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "AS") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			NK[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "NK") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			WN[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "WN") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			DL[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "DL") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			EV[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "EV") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			HA[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "HA") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			MQ[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "MQ") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			VX[i] <- nrow(subset(flightsFrame, (flightsFrame$YEAR == y) & (flightsFrame$MONTH == m) & (flightsFrame$DAY == d) & (flightsFrame$AIRLINE == "VX") & (((flightsFrame$DIVERTED == 1) | (flightsFrame$CANCELLED == 1) | ((flightsFrame$DEPARTURE_DELAY > 0) & (flightsFrame$ARRIVAL_DELAY > 0))))))
			
			i <- i + 1
		}
	}	
}

#plotting the individual barplots for each flight
barplot(UA , main = "United Air Lines Inc.", ylab = "Number of Flights", xlab = "Days")
barplot(AA , main = "American Airlines Inc.", ylab = "Number of Flights", xlab = "Days" )
barplot(US , main = "US Airways Inc.", ylab = "Number of Flights", xlab = "Days" )
barplot(F9 , main = "Frontier Airlines Inc.", ylab = "Number of Flights", xlab = "Days" )
barplot(B6 , main = "JetBlue Airways", ylab = "Number of Flights", xlab = "Days" )
barplot(OO , main = "Skywest Airlines Inc.", ylab = "Number of Flights", xlab = "Days" )
barplot(AS , main = "Alaska Airlines Inc.", ylab = "Number of Flights", xlab = "Days" )
barplot(NK , main = "Spirit Air Lines", ylab = "Number of Flights", xlab = "Days" )
barplot(WN , main = "Southwest Airlines Co.", ylab = "Number of Flights", xlab = "Days" )
barplot(DL , main = "Delta Air Lines Inc.", ylab = "Number of Flights", xlab = "Days" )
barplot(EV , main = "Atlantic Southeast Airlines", ylab = "Number of Flights", xlab = "Days" )
barplot(HA , main = "Hawaiian Airlines Inc.", ylab = "Number of Flights", xlab = "Days" )
barplot(MQ , main = "American Eagle Airlines Inc.", ylab = "Number of Flights", xlab = "Days" )
barplot(VX , main = "Virgin America", ylab = "Number of Flights", xlab = "Days" )

#to give a better insight to the data, plotting scatter plots also
plot(UA)
plot(AA)
plot(US)
plot(F9)
plot(B6)
plot(OO)
plot(AS)
plot(NK)
plot(WN)
plot(DL)
plot(EV)
plot(HA)
plot(MQ)
plot(VX)

cat("As seen from the above graphs, at a certain point of time, the performance of the United Air Lines Inc. declined. Therefore we can consider it to be the airline Y.
	American Airlines Inc. on the other hand has shown an outsanding performance as observed from the graphs. Therefore, it is the airline X that supposedly replaced airline Y.")