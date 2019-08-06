setwd("~/Documents/github")

library(ggplot2)

pm25 <- readRDS('DataScience/exploratory_data_analysis/week4/data/summarySCC_PM25.rds')
classified <- readRDS('DataScience/exploratory_data_analysis/week4/data/Source_Classification_Code.rds')

total_1999 <- 0
total_2002 <- 0
total_2005 <- 0
total_2008 <- 0

for (i in 1:nrow(pm25)){
  if(pm25$year[i] == 1999){
    total_1999 <- total_1999 + pm25$Emissions[i]
  } 
  else if (pm25$year[i] == 2002){
    total_2002 <- total_2002 + pm25$Emissions[i]
  }
  else if (pm25$year[i] == 2005){
    total_2005 <- total_2005 + pm25$Emissions[i]
  } else {
    total_2008 <- total_2008 + pm25$Emissions[i]
  }
}

pm25_trend <- data.frame(matrix(vector(), 4, 2, dimnames=list(c(), c("year", "annual_emission"))))

pm25_trend$year <- (c(1999, 2002, 2005, 2008))

pm25_trend$annual_emission <- (c(total_1999, total_2002, total_2005, total_2008))

plot(pm25_trend$year, pm25_trend$annual_emission, col="deeppink", lty = 1, xlab = "Year", ylab = "Emission (tons)", xaxt = "none")
lines(pm25_trend$year, pm25_trend$annual_emission, col="deeppink", type = "l", lty = 1, xlab = "Year", ylab = "Emission (tons)",xaxt = "none")
title("Annual PM2.5 Emissions from 1999 to 2008")
axis(1, seq(1999,2008, 3),las=1, font=1,cex.axis=0.8)

dev.copy(png, file="DataScience/exploratory_data_analysis/week4/plot1.png", height=480, width=480)
dev.off()
