library(plotly)
library(quantmod)
#Change current directory
setwd("C:/Users/piyus/Downloads/DA assignments/DA Assignment 3")

seriesdata <- read.csv("MER_T12_06.csv")
seriesdata$YYYY <- trunc(seriesdata$YYYYMM/100)
seriesdata$MM <- (seriesdata$YYYYMM%%100)
seriesdata$MM <- with(seriesdata,ifelse(MM <= 9,paste("0",MM,sep=""),MM))
seriesdata$YYYYMM <-paste(seriesdata$YYYY,seriesdata$MM,sep="-")
yeardata <- subset(seriesdata,MM!=13,select=c("YYYYMM","Value","MSN"))
msns <- as.vector(unique(yeardata$MSN))
#View(yeardata)
coli=1
buttonlist <- list(list(method = "restyle",args = list("visible", temptruthlist),label = "All Sectors"))
for(i in msns)
{
  newdata <- subset(yeardata,MSN==i)
  x <- newdata$YYYYMM
  y <- as.numeric(newdata$Value)

  if(coli==1)
      p <- plot_ly(x = x, y = y,type="scatter",mode = 'lines',name=i)
  else
    p <- add_trace(p , y = y,type="scatter",mode = 'lines',name=i)#,visible=F)

  coli <- coli + 1
}

p <- layout(p,title = "Emission of CO2",xaxis = list(title="Date",rangeslider = list(type = "date")),yaxis = list(title = "CO2 Emitted"))
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
#chart_link = api_create(p, filename="timeseries/1")
#chart_link
