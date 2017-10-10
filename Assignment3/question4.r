#we have decided the best way to visualize twitter dat ais using word cloud
#importing the required lbraries
library(tm)
library(SnowballC)
library(wordcloud)
library(plyr)

demonetizationFrame <- read.csv('D:/Data Analytics/Assingment 3/question 4/demonetization.csv', header = TRUE, sep = ",",  stringsAsFactors = FALSE)
demonetizationFrame$text <- iconv(demonetizationFrame$text, "latin1", "utf-8", sub="")
demonetizationCorpus <- Corpus(VectorSource(demonetizationFrame$text))

#plotting word cloud
tdm = TermDocumentMatrix(demonetizationCorpus, control = list(removePunctuation = TRUE, stopwords = c(stopwords("english"), "https", "smt", "amp", "uodwxdpmmg", "says"), removeNumbers = TRUE, tolower = TRUE))
m = as.matrix(tdm)
word_freqs = sort(rowSums(m), decreasing=TRUE)
dm = data.frame(word=names(word_freqs), freq=word_freqs)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), max.words = 100)

cat("The word demonetization occurs the most since it is the most prominent word in the word cloud.")