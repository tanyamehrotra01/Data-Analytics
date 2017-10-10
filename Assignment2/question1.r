myDataFrame <- read.csv(file="f:/Data Analytics/Assingment 2/pokemon.csv", header=TRUE, sep=",")
q <- myDataFrame[sample(nrow(myDataFrame), nrow(myDataFrame)*0.70), ] #simple random sampling of 70% of data

sys.sample <- function(N,n) #function for systematic sampling
{
	k = ceiling(N/n)
	r = sample(1,1)
	sys.samp = seq(r , r + k*(n - 1),k) #k = 3 here
}
pokemon_sys <- sys.sample(721,241) #defining N and n and passing it in the function
pokemon_sys <- subset(myDataFrame, Number %in% pokemon_sys)


p <- subset(myDataFrame, myDataFrame$Pr_Male >= 0.5 && myDataFrame$hasGender == TRUE) #stratified random sampling where the data is male or female



random_primary_types <- unique(subset(myDataFrame, select = c(Type_1))) #clustered sampling based on primary type (Type_1) 
																		 #There are total 18 types 
random_primary_types <- random_primary_types[sample(nrow(random_primary_types), 0.7*nrow(random_primary_types)), ] #choosing 70% of type_1

#we find out that there are 12 unique types in primary types so we define 12 frames (brute force method)

#frame0 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[0]) heading
frame1 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[1])
frame2 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[2])
frame3 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[3])
frame4 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[4])
frame5 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[5])
frame6 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[6])
frame7 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[7])
frame8 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[8])
frame9 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[9])
frame10 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[10])
frame11 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[11])
frame12 <- subset(myDataFrame, myDataFrame$Type_1 == random_primary_types[12])


hist(p$Total) #stratified random sampling
hist(pokemon_sys$Total) #systematic sampling
hist(q$Total) #simple random sampling
hist(myDataFrame$Total)







