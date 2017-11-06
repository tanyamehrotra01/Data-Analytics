library(arules)
writing <- read.csv(file="C:/PES/5TH SEM/DA/week7/handwriting_recognition.csv",header=T,na.strings=c(""))

m <- subset(writing, select=c(Recognition,Gender,Profession,Freq))
n <- subset(writing, select=c(Recognition,Gender,Profession))

fin <- head(n,1)
m$Freq[1] <- m$Freq[1] - 1

for(i in 1:nrow(m))
{
  for(j in 1:m$Freq[i])
  {
      #print(n[i,])
     fin <- rbind(fin,n[i,])
  }
}
summary(fin)

rules.all <- apriori(fin)
#rules with default settings
inspect(rules.all) 

#part 1
rules1 <- apriori(fin, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(lhs=c("Profession=Artist","Gender=Female"),rhs=c("Recognition=Recognized")), control = list (verbose=F))
inspect(rules1)

#part 2
rules2 <- apriori(fin, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(lhs=c("Profession=Engineer"),rhs=c("Gender=Male")), control = list (verbose=F))
inspect(rules2)

#part 3
rules3 <- apriori(fin, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(lhs=c("Profession=Actor","Recognition=Recognized"),rhs=c("Gender=Female")), control = list (verbose=F))
inspect(rules3)

#part 4
rules1 <- apriori(fin, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(lhs=c("Profession=Doctor","Gender=Male"),rhs=c("Recognition=Unrecognized")), control = list (verbose=F))
inspect(rules1)
