myDataFrame <- read.csv(file="f:/Data Analytics/Assingment 2/pokemon.csv", header=TRUE, sep=",")

#Generation and Pr_Male are dropped
pca <- prcomp(subset(myDataFrame, retx = TRUE, select = c(Total,HP,Attack,Defense,Sp_Atk,Sp_Def,Speed,Height_m,Weight_kg,Catch_Rate)))
summary(pca)

Var <- NULL
VarCount <- 1

for (i in pca[1])
{
	for (j in i)
	{
		Var[VarCount] <- j
		VarCount <- VarCount + 1
	}
}

plot(Var, col = "red" , type = "o")


# pca$x[,1],pca$x[,2]

