setwd("~/Documents/github")

library(ggplot2)

pm25 <- readRDS('DataScience/exploratory_data_analysis/week4/data/summarySCC_PM25.rds')
classified <- readRDS('DataScience/exploratory_data_analysis/week4/data/Source_Classification_Code.rds')

#join table
joinDf<-merge(x=pm25,y=classified,by="SCC",all=TRUE)

bc_1999 <- 0
bc_2002 <- 0
bc_2005 <- 0
bc_2008 <- 0

la_1999 <- 0
la_2002 <- 0
la_2005 <- 0
la_2008 <- 0


for (i in 1:nrow(joinDf)){
  if((toString(joinDf$SCC.Level.Three[i]) =="Motor Vehicles: SIC 371" || toString(joinDf$SCC.Level.Three[i]) =="Motor Vehicle Fires") && !is.na(joinDf$year[i])){
    if(joinDf$year[i] == "1999"){
      if(joinDf$fips[i] == "24510"){
        bc_1999 <- bc_1999 +joinDf$Emissions[i]
      } else if (joinDf$fips[i] == "06037"){
        la_1999 <- la_1999 + joinDf$Emissions[i]
      }
    }
    else if(joinDf$year[i] == "2002"){
      if(joinDf$fips[i] == "24510"){
        bc_2002 <- bc_2002 + joinDf$Emissions[i]
      } else if (joinDf$fips[i] == "06037"){
        la_2002 <- la_2002 + joinDf$Emissions[i]
      }
    }
    else if(joinDf$year[i] == "2005"){
      if(joinDf$fips[i] == "24510"){
        bc_2005 <- bc_2005 + joinDf$Emissions[i]
      } else if (joinDf$fips[i] == "06037"){
        la_2005 <- la_2005 + joinDf$Emissions[i]
      }
    }
    else if(joinDf$year[i] == "2008"){
      if(joinDf$fips[i] == "24510"){
        bc_2008 <- bc_2008 + joinDf$Emissions[i]
      } else if (joinDf$fips[i] == "06037"){
        la_2008 <- la_2008 + joinDf$Emissions[i]
      }
    }
  }
  cat("on ", toString(i) ,bc_1999, bc_2002, bc_2005, bc_2008, la_1999, la_2002, la_2005, la_2008, "\n")
}

pm25_trend <- data.frame(matrix(vector(), 4, 3, dimnames=list(c(), c("year", "bc_emission", "la_emission"))))

pm25_trend$year <- (c(1999, 2002, 2005, 2008))

pm25_trend$bc_emission <- (c(bc_1999, bc_2002, bc_2005, bc_2008))

pm25_trend$la_emission <- (c(la_1999, la_2002, la_2005, la_2008))

plot6 <- ggplot() +  
  geom_line(data=pm25_trend, aes(year, bc_emission, colour = "Baltimore City"), size = 0.5)+
  geom_line(data=pm25_trend, aes(year, la_emission, colour = "Los Angeles"), size = 0.5)+
  scale_colour_manual(values = c(
    'Baltimore City' = 'blue',
    'Los Angeles' = 'red'
  ))+  
  scale_y_continuous(name = expression("PM2.5 Emissions (tons)"), limits = c(0, 100))+
  labs(y = "PM2.5 Emissions (tons)",
       x = "Year",
       colour = 'Legend') +
  ggtitle("Annual PM2.5 Emissions from 1999 to 2008 by Motor Vehicles","Baltimore City and Los Angeles") +
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

plot6

dev.copy(png, file="DataScience/exploratory_data_analysis/week4/plot6.png", height=480, width=480)
dev.off()


