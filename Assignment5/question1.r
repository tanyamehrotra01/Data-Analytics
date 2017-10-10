GOTFrame <- read.csv("D:/Data Analytics/Assignment 5/game_of_thrones_data/A5_Q1/got_character_train.csv", header = TRUE, sep = ",")
# 1 (a)
#Filling age with median values
GOTFrame$age[is.na(GOTFrame$age)] <- median(GOTFrame$age, na.rm=TRUE)
#filling book mentions, if dead relations, gender, number of dead relations, nobility, popularity,
#maritial status, info about alive spouse, mother, father, heir

GOTFrame[c("isAliveFather")][is.na(GOTFrame[c("isAliveFather")])] <- -1
GOTFrame[c("boolDeadRelations")][is.na(GOTFrame[c("boolDeadRelations")])] <- -1
GOTFrame[c("book1")][is.na(GOTFrame[c("book1")])] <- -1
GOTFrame[c("book2")][is.na(GOTFrame[c("book2")])] <- -1
GOTFrame[c("book3")][is.na(GOTFrame[c("book3")])] <- -1
GOTFrame[c("book4")][is.na(GOTFrame[c("book4")])] <- -1
GOTFrame[c("book5")][is.na(GOTFrame[c("book5")])] <- -1
GOTFrame[c("male")][is.na(GOTFrame[c("male")])] <- -1
GOTFrame[c("numDeadRelations")][is.na(GOTFrame[c("numDeadRelations")])] <- -1
GOTFrame[c("isNoble")][is.na(GOTFrame[c("isNoble")])] <- -1
GOTFrame[c("popularity")][is.na(GOTFrame[c("popularity")])] <- -1
GOTFrame[c("isMarried")][is.na(GOTFrame[c("isMarried")])] <- -1
GOTFrame[c("isAliveSpouse")][is.na(GOTFrame[c("isAliveSpouse")])] <- -1
GOTFrame[c("isAliveMother")][is.na(GOTFrame[c("isAliveMother")])] <- -1
GOTFrame[c("isAliveHeir")][is.na(GOTFrame[c("isAliveHeir")])] <- -1
GOTFrame[c("isPopular")][is.na(GOTFrame[c("isPopular")])] <- -1

# 1 (b)
numAlive <- nrow(subset(GOTFrame, GOTFrame$isAlive == 1))
numDead <- nrow(subset(GOTFrame, GOTFrame$isAlive == 0))
# Pie Chart with Percentages
slices <- c(numAlive, numDead) 
lbls <- c("Characters Alive", "Characters Dead")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Pie Chart of Alive and Dead Characters")
#upsampling of dead people
library(caret)
table(GOTFrame$isAlive)
#OverSampling - 25:75 (Dead : Alive)
GOTFrame$isAlive <- as.factor(GOTFrame$isAlive)
set.seed(3)
GOTFrame <- upSample(x = subset(GOTFrame, select = -c(isAlive)), y = GOTFrame$isAlive, yname = "isAlive")
table(GOTFrame$isAlive)
#Pie chart after upsampling
numAlive1 <- nrow(subset(GOTFrame, GOTFrame$isAlive == 1))
numDead1 <- nrow(subset(GOTFrame, GOTFrame$isAlive == 0))
# Pie Chart with Percentages
slices <- c(numAlive1, numDead1) 
lbls <- c("Characters Alive", "Characters Dead")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Pie Chart of Alive and Dead Characters")


# 1 (c)
#Logistic regression is a pretty flexible method. It can readily use as independent variables categorical variables.
#Most software that use Logistic regression should enable the use of categorical variables.
#For example, let's say one of our categorical variable is temperature defined into three categories: cold/mild/hot. 
#We could interpret that as three separate dummy variables each with a value of 1 or 0. But, the software uses a single 
#categorical variable instead with text value cold/mild/hot. And, the logit regression would derive coefficient (or constant)
#for each of the three temperature conditions. 
#The main benefit of grouping categorical variable categories into a single categorical variable is model efficiency. 
#A single column in the model can handle as many categories as needed for a single categorical variable. If instead, a dummy 
#variable is used for each categories of a categorical variable the model can quickly grow to have numerous columns that are 
#superfluous given the mentioned alternative.

# 1 (d)
install.packages('caTools')
library(caTools)
model1 <- glm (isAlive ~ male+boolDeadRelations+age+numDeadRelations+book1+book2+book3+book4+book5+isAliveMother+isAliveFather+isAliveHeir+isAliveSpouse+isMarried+isNoble+popularity+isPopular, family = binomial("logit"), data = GOTFrame)
summary(model1) #AIC : 2553.9

# 1 (e)
#From the summary, we note down the important parameters 
model2 <- glm (isAlive ~ male+boolDeadRelations+book1+book2+book3+book4+isAliveMother+popularity, family = binomial("logit"), data = GOTFrame)
summary(model2) #AIC : 2555.2

#model1 has less AIC value than model2, so model1 is better
# 1 (f)
GOTFrame_test <- read.csv("D:/Data Analytics/Assignment 5/game_of_thrones_data/A5_Q1/got_character_test.csv", header = TRUE, sep = ",")
GOTFrame_test$age[is.na(GOTFrame_test$age)] <- median(GOTFrame_test$age, na.rm=TRUE)
#filling book mentions, if dead relations, gender, number of dead relations, nobility, popularity,
#maritial status, info about alive spouse, mother, father, heir

GOTFrame_test[c("isAliveFather")][is.na(GOTFrame_test[c("isAliveFather")])] <- -1
GOTFrame_test[c("boolDeadRelations")][is.na(GOTFrame_test[c("boolDeadRelations")])] <- -1
GOTFrame_test[c("book1")][is.na(GOTFrame_test[c("book1")])] <- -1
GOTFrame_test[c("book2")][is.na(GOTFrame_test[c("book2")])] <- -1
GOTFrame_test[c("book3")][is.na(GOTFrame_test[c("book3")])] <- -1
GOTFrame_test[c("book4")][is.na(GOTFrame_test[c("book4")])] <- -1
GOTFrame_test[c("book5")][is.na(GOTFrame_test[c("book5")])] <- -1
GOTFrame_test[c("male")][is.na(GOTFrame_test[c("male")])] <- -1
GOTFrame_test[c("numDeadRelations")][is.na(GOTFrame_test[c("numDeadRelations")])] <- -1
GOTFrame_test[c("isNoble")][is.na(GOTFrame_test[c("isNoble")])] <- -1
GOTFrame_test[c("popularity")][is.na(GOTFrame_test[c("popularity")])] <- -1
GOTFrame_test[c("isMarried")][is.na(GOTFrame_test[c("isMarried")])] <- -1
GOTFrame_test[c("isAliveSpouse")][is.na(GOTFrame_test[c("isAliveSpouse")])] <- -1
GOTFrame_test[c("isAliveMother")][is.na(GOTFrame_test[c("isAliveMother")])] <- -1
GOTFrame_test[c("isAliveHeir")][is.na(GOTFrame_test[c("isAliveHeir")])] <- -1
GOTFrame_test[c("isPopular")][is.na(GOTFrame_test[c("isPopular")])] <- -1


predict1 <- predict(model1, GOTFrame_test, type = "response")
table(GOTFrame_test$isAlive, predict1 > 0.5)

#Matrix is 
# 88	49
# 121	229

predict2 <- predict(model2, GOTFrame_test, type = "response")
table(GOTFrame_test$isAlive, predict2 > 0.5)

#Matrix is
# 92	45
# 119 	231

# 1 (g)
install.packages('ROCR')
library(ROCR)
install.packages("pROC")
library(pROC)
ROCRpred1 <- prediction(predict1, GOTFrame_test$isAlive)
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')
plot(ROCRperf1, colorize = TRUE)
print(roc(GOTFrame_test$isAlive, predict1)) #0.7126

ROCRpred2 <- prediction(predict2, GOTFrame_test$isAlive)
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
plot(ROCRperf2, colorize = TRUE)
print(roc(GOTFrame_test$isAlive, predict2))  #0.7198

#area under ROC of model1 is less thanmodel2, so model2 is better.
