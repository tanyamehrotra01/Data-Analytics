#we have decided the best way to represent the given data is through a stacked bar plot.
library(ggplot2)
library(RColorBrewer)

myDataFrame <- read.csv(file="D:/Data Analytics/Assingment 3/question 3/business_rankings.csv", header=TRUE, sep=",")
#choosing the top 20 countires of the dataset with respect to the first category
myDataFrame <- myDataFrame[c(1:20),]

#take reciprocal of ranks
#for 3point precision multiple by 100
myDataFrame[,c(2:5)] <- 1/(myDataFrame[,c(2:5)])*100

myDataFrame$Economy <- as.character(myDataFrame$Economy)

#plot for Ease of Doing Business Ratings
ggplot(data=myDataFrame, aes(x=myDataFrame$Economy, y=myDataFrame$Ease.of.Doing.Business.Rank, width=.6)) +
  geom_bar(stat="identity", position="identity") + labs(x="Country", y="Ease of Doing Business") +
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Ease of Doing Business Ratings")

#plot for Starting a Business
ggplot(data=myDataFrame, aes(x=myDataFrame$Economy, y=myDataFrame$Starting.a.Business, width=.6)) +
  geom_bar(stat="identity", position="identity") + labs(x="Country", y="Starting a Business") +
  theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Starting a Business Rating")

#plot for Dealing with Construction
ggplot(data=myDataFrame, aes(x=myDataFrame$Economy, y=myDataFrame$Dealing.with.Construction.Permits, width=.6)) +
  geom_bar(stat="identity", position="identity") + labs(x="Country", y="Dealing with Construction") +
  theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Dealing with Construction Ratings")

#plot for Protecting Minority Investors
ggplot(data=myDataFrame, aes(x=myDataFrame$Economy, y=myDataFrame$Protecting.Minority.Investors, width=.6)) +
  geom_bar(stat="identity", position="identity") + labs(x="Country", y="Protecting Minority Investors") +
  theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Protecting Minority Investors")

#stacked_bar_plot representing all the categories
library(reshape2)
businessDataFrame <- melt(myDataFrame, id.var="Economy")
ggplot(businessDataFrame, aes(x = Economy, y = value, fill = variable)) +
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Stacked_bar_plot_analysis")+ coord_flip() + scale_fill_brewer(palette = 14) + labs(title = "Stacked Bar Plot for Business Ranking", 
    y = "Performance Points", x = "Country", fill = "Stage of Change")