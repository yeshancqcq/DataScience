setwd("~/Documents/github")

library(ggplot2)

pm25 <- readRDS('DataScience/exploratory_data_analysis/week4/data/summarySCC_PM25.rds')
classified <- readRDS('DataScience/exploratory_data_analysis/week4/data/Source_Classification_Code.rds')

#join table
joinDf<-merge(x=pm25,y=classified,by="SCC",all=TRUE)

coal_1999 <- 0
coal_2002 <- 0
coal_2005 <- 0
coal_2008 <- 0

# Function that can check if the last 4 characters of a string is Coal
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

for (i in 1:nrow(joinDf)){
    if(substrRight(toString(joinDf$EI.Sector[i]),4)=="Coal" && !is.na(joinDf$year[i])){
      if(joinDf$year[i] == 1999){
        coal_1999 <- coal_1999 + joinDf$Emissions[i]
      } 
      else if (joinDf$year[i] == 2002){
        coal_2002 <-coal_2002 + joinDf$Emissions[i]
      }
      else if (joinDf$year[i] == 2005){
        coal_2005 <- coal_2005 + joinDf$Emissions[i]
      } else if (joinDf$year[i] == 2008) {
        coal_2008 <- coal_2008 + joinDf$Emissions[i]
      }
    }
  cat("on ", toString(i) ,"\n")
}

pm25_trend <- data.frame(matrix(vector(), 4, 2, dimnames=list(c(), c("year", "annual_emission"))))

pm25_trend$year <- (c(1999, 2002, 2005, 2008))

pm25_trend$annual_emission <- (c(coal_1999, coal_2002, coal_2005, coal_2008))

plot(pm25_trend$year, pm25_trend$annual_emission, col="deeppink", lty = 1, xlab = "Year", ylab = "Coal Emission (tons)", xaxt = "none")
lines(pm25_trend$year, pm25_trend$annual_emission, col="deeppink", type = "l", lty = 1, xlab = "Year", ylab = "Emission (tons)",xaxt = "none")
title("Annual PM2.5 Emissions from 1999 to 2008","Coal combustion-related sources")
axis(1, seq(1999,2008, 3),las=1, font=1,cex.axis=0.8)

dev.copy(png, file="DataScience/exploratory_data_analysis/week4/plot4.png", height=480, width=480)
dev.off()


