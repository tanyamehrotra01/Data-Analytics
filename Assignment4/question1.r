#QUESTION1

cancerFrame1 <- read.csv("D:/Data Analytics/Assignment 4/question 1/cancer_2015(2).csv", header = TRUE, sep = ",")

#importing the required library
library(zoo)
na.aggregate(cancerFrame1)
#function used to replace the missing values with the mean of the corresponding attribute
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
replace(cancerFrame1, TRUE, lapply(cancerFrame1, NA2mean))
cancerFrame1 <- lapply(cancerFrame1, NA2mean)

#filling the missing value using linear interpolation
cancerFrame2 <- read.csv("D:/Data Analytics/Assignment 4/question 1/cancer_2015(2).csv", header = TRUE, sep = ",")
cancerFrame2$Outpatients.TMH <- na.approx(cancerFrame2$Outpatients.TMH)
cancerFrame2$Outpatients.TMH
cancerFrame2$New.Registrations.ACTREC <- na.approx(cancerFrame2$New.Registrations.ACTREC)
cancerFrame2$New.Registrations.ACTREC
cancerFrame2$Laboratory.Investigations.ACTREC <- na.approx(cancerFrame2$Laboratory.Investigations.ACTREC)
cancerFrame2$Laboratory.Investigations.ACTREC

#filling using quadratic interpolation
cancerFrame3 <- read.csv("D:/Data Analytics/Assignment 4/question 1/cancer_2015(2).csv", header = TRUE, sep = ",")
cancerFrame3$Outpatients.TMH <- na.spline(cancerFrame3$Outpatients.TMH)
cancerFrame3$Outpatients.TMH
cancerFrame3$New.Registrations.ACTREC <- na.spline(cancerFrame3$New.Registrations.ACTREC)
cancerFrame3$New.Registrations.ACTREC
cancerFrame3$Laboratory.Investigations.ACTREC <- na.spline(cancerFrame3$Laboratory.Investigations.ACTREC)
cancerFrame3$Laboratory.Investigations.ACTREC

#Linear Regression
cancerFrame4 <- read.csv("D:/Data Analytics/Assignment 4/question 1/cancer_2015(2).csv", header = TRUE, sep = ",")
#converting the Month column into an integer corresponding to the month number and plotting it against the attributes having missing values.
#This column is taken because it is the only independent column
cancerFrame4$Month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
#finding the m and c values for the three attributes
linearMod1 <- lm( cancerFrame4$Month ~ cancerFrame4$Outpatients.TMH) 
print("For Outpatients TMH")
print(linearMod1)

linearMod2 <- lm( cancerFrame4$Month ~ cancerFrame4$New.Registrations.ACTREC) 
print("For New Registrations ACTREC")
print(linearMod2)

linearMod3 <- lm( cancerFrame4$Month ~ cancerFrame4$Laboratory.Investigations.ACTREC) 
print("For Laboratory Investigations ACTREC")
print(linearMod3)

for(i in 1:nrow(cancerFrame4))
{
	if(is.na(cancerFrame4$Outpatients.TMH[i]))
		cancerFrame4$Outpatients.TMH[i] = 24.092021 - 0.004306*cancerFrame4$Month[i]
	if(is.na(cancerFrame4$New.Registrations.ACTREC[i]))
		cancerFrame4$New.Registrations.ACTREC[i] = -15.2727 + 0.4545*cancerFrame4$Month[i]
	if(is.na(cancerFrame4$Laboratory.Investigations.ACTREC[i]))
		cancerFrame4$Laboratory.Investigations.ACTREC[i] = 6.385e+00 + 9.162e-05*cancerFrame4$Month[i]
}

#mice
library(mice)

cancerFrame5 <- read.csv("D:/Data Analytics/Assignment 4/question 1/cancer_2015(2).csv", header = TRUE, sep = ",")


miceCancerFrame <- mice(cancerFrame5, m = 5 , maxit = 50)
miceCancerFrame1 <- complete(miceCancerFrame, 1)
miceCancerFrame2 <- complete(miceCancerFrame, 2)
miceCancerFrame3 <- complete(miceCancerFrame, 3)
miceCancerFrame4 <- complete(miceCancerFrame, 4)
miceCancerFrame5 <- complete(miceCancerFrame, 5)

print(miceCancerFrame1[4:7,5:7])
print(miceCancerFrame2[4:7,5:7])
print(miceCancerFrame3[4:7,5:7])
print(miceCancerFrame4[4:7,5:7])
print(miceCancerFrame5[4:7,5:7])


dev.new()
x <- cancerFrame1$Month
q <- c("Mean", "Linear Interpolation", "Quadratic Interpolation", "Linear Regression","1st Imputation", "2nd Imputation", "3rd Imputation", "4th Imputation", "5th Imputation")
plot(cancerFrame1$Outpatients.TMH, xaxt = "n", xlab = "Month",ylab = "Outpatients TMH", main = "Missing values of Outpatients TMH")
axis(1,at = 1:length(x),labels = x)
lines(cancerFrame2$Outpatients.TMH, col = "blue")
lines(cancerFrame3$Outpatients.TMH, col = "red")
lines(cancerFrame4$Outpatients.TMH, col = "brown")
lines(miceCancerFrame1$Outpatients.TMH, col = "green")
lines(miceCancerFrame2$Outpatients.TMH, col = "yellow")
lines(miceCancerFrame3$Outpatients.TMH, col = "pink")
lines(miceCancerFrame4$Outpatients.TMH, col = "purple")
lines(miceCancerFrame5$Outpatients.TMH, col = "orange")

legend("topright", legend=q,lty=1:1,cex=0.64,col=c("blue","red","brown","green","yellow","pink","purple","orange"))

dev.new()
x <- cancerFrame1$Month
q <- c("Mean", "Linear Interpolation", "Quadratic Interpolation", "Linear Regression", "1st Imputation", "2nd Imputation", "3rd Imputation", "4th Imputation", "5th Imputation")
plot(cancerFrame1$New.Registrations.ACTREC, xaxt = "n", xlab = "Month",ylab = "New.Registrations.ACTREC", main = "Missing values of New Registrations ACTREC")
axis(1,at = 1:length(x),labels = x)
lines(cancerFrame2$New.Registrations.ACTREC, col = "blue")
lines(cancerFrame3$New.Registrations.ACTREC, col = "red")
lines(cancerFrame4$New.Registrations.ACTREC, col = "brown")
lines(miceCancerFrame1$New.Registrations.ACTREC, col = "green")
lines(miceCancerFrame2$New.Registrations.ACTREC, col = "yellow")
lines(miceCancerFrame3$New.Registrations.ACTREC, col = "pink")
lines(miceCancerFrame4$New.Registrations.ACTREC, col = "purple")
lines(miceCancerFrame5$New.Registrations.ACTREC, col = "orange")

legend("topright", legend=q,lty=1:1,cex=0.64,col=c("blue","red","brown","green","yellow","pink","purple","orange"))

dev.new()
x <- cancerFrame1$Month
q <- c("Mean", "Linear Interpolation", "Quadratic Interpolation", "Linear Regression", "1st Imputation", "2nd Imputation", "3rd Imputation", "4th Imputation", "5th Imputation")
plot(cancerFrame1$Laboratory.Investigations.ACTREC, xaxt = "n", xlab = "Month",ylab = "Laboratory.Investigations.ACTREC", main = "Missing values of Laboratory Investigations ACTREC")
axis(1,at = 1:length(x),labels = x)
lines(cancerFrame2$Laboratory.Investigations.ACTREC, col = "blue")
lines(cancerFrame3$Laboratory.Investigations.ACTREC, col = "red")
lines(cancerFrame4$Laboratory.Investigations.ACTREC, col = "brown")
lines(miceCancerFrame1$Laboratory.Investigations.ACTREC, col = "green")
lines(miceCancerFrame2$Laboratory.Investigations.ACTREC, col = "yellow")
lines(miceCancerFrame3$Laboratory.Investigations.ACTREC, col = "pink")
lines(miceCancerFrame4$Laboratory.Investigations.ACTREC, col = "purple")
lines(miceCancerFrame5$Laboratory.Investigations.ACTREC, col = "orange")

legend("topright", legend=q,lty=1:1,cex=0.64,col=c("blue","red","brown","green","yellow","pink","purple","orange"))

