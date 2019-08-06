setwd("~/Documents/github")

library(ggplot2)

pm25 <- readRDS('DataScience/exploratory_data_analysis/week4/data/summarySCC_PM25.rds')
classified <- readRDS('DataScience/exploratory_data_analysis/week4/data/Source_Classification_Code.rds')

#join table
joinDf<-merge(x=pm25,y=classified,by="SCC",all=TRUE)

total_1999 <- 0
total_2002 <- 0
total_2005 <- 0
total_2008 <- 0


for (i in 1:nrow(joinDf)){
  if((toString(joinDf$SCC.Level.Three[i]) =="Motor Vehicles: SIC 371" || toString(joinDf$SCC.Level.Three[i]) =="Motor Vehicle Fires") && !is.na(joinDf$year[i])){
    if(joinDf$year[i] == 1999 && joinDf$fips[i] == 24510){
      total_1999 <- total_1999 +joinDf$Emissions[i]
    } 
    else if (joinDf$year[i] == 2002 && joinDf$fips[i] == 24510){
      total_2002 <- total_2002 + joinDf$Emissions[i]
    }
    else if (joinDf$year[i] == 2005 && joinDf$fips[i] == 24510){
      total_2005 <- total_2005 + joinDf5$Emissions[i]
    } else if (joinDf$year[i] == 2008 && joinDf$fips[i] == 24510) {
      total_2008 <- total_2008 + joinDf$Emissions[i]
    }
  }
  cat("on ", toString(i) ,"\n")
}

pm25_trend <- data.frame(matrix(vector(), 4, 2, dimnames=list(c(), c("year", "annual_emission"))))

pm25_trend$year <- (c(1999, 2002, 2005, 2008))

pm25_trend$annual_emission <- (c(total_1999, total_2002, total_2005, total_2008))

plot5 <- ggplot() +  
  geom_line(data=pm25_trend, aes(year, annual_emission), size = 0.5, colour = "red")+  
  scale_y_continuous(name = expression("PM2.5 Emissions (tons)"), limits = c(0, 15))+
  labs(y = "PM2.5 Emissions (tons)",
       x = "Year",
       colour = 'Legend') +
  ggtitle("Annual PM2.5 Emissions from 1999 to 2008 by Motor Vehicles","Baltimore City") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(colour = "black"),
        axis.title.y=element_text(size=10),
        axis.line= element_line(colour = "black"),
        axis.title.x=element_text(size=10),
        legend.justification = c(0, 0),
        legend.position = c(0.75, 0.75),
        legend.background = element_rect(colour = "black"),
        legend.key = element_rect(colour = "white", fill = NA),
        legend.title = element_blank(),
        legend.text=element_text(size=rel(0.83))
  ) +
  scale_x_continuous(breaks=seq(1999, 2008, 3))

plot5

dev.copy(png, file="DataScience/exploratory_data_analysis/week4/plot5.png", height=480, width=480)
dev.off()

