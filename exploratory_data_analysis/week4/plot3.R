setwd("~/Documents/github")

library(ggplot2)

pm25 <- readRDS('DataScience/exploratory_data_analysis/week4/data/summarySCC_PM25.rds')
classified <- readRDS('DataScience/exploratory_data_analysis/week4/data/Source_Classification_Code.rds')

point_1999 <- 0
nonpoint_1999 <- 0
onroad_1999 <- 0
nonroad_1999 <- 0

point_2002 <- 0
nonpoint_2002 <- 0
onroad_2002 <- 0
nonroad_2002 <- 0

point_2005 <- 0
nonpoint_2005 <- 0
onroad_2005 <- 0
nonroad_2005 <- 0

point_2008 <- 0
nonpoint_2008 <- 0
onroad_2008 <- 0
nonroad_2008 <- 0

for (i in 1:nrow(pm25)){
  if(pm25$type[i]=="POINT"){
    if(pm25$year[i] == 1999 && pm25$fips[i] == 24510){
      point_1999 <- point_1999 + pm25$Emissions[i]
    } 
    else if (pm25$year[i] == 2002 && pm25$fips[i] == 24510){
      point_2002 <- point_2002 + pm25$Emissions[i]
    }
    else if (pm25$year[i] == 2005 && pm25$fips[i] == 24510){
      point_2005 <- point_2005 + pm25$Emissions[i]
    } else if (pm25$year[i] == 2008 && pm25$fips[i] == 24510) {
      point_2008 <- point_2008 + pm25$Emissions[i]
    }
  } else if(pm25$type[i]=="NONPOINT"){
    if(pm25$year[i] == 1999 && pm25$fips[i] == 24510){
      nonpoint_1999 <- nonpoint_1999 + pm25$Emissions[i]
    } 
    else if (pm25$year[i] == 2002 && pm25$fips[i] == 24510){
      nonpoint_2002 <- nonpoint_2002 + pm25$Emissions[i]
    }
    else if (pm25$year[i] == 2005 && pm25$fips[i] == 24510){
      nonpoint_2005 <- nonpoint_2005 + pm25$Emissions[i]
    } else if (pm25$year[i] == 2008 && pm25$fips[i] == 24510) {
      nonpoint_2008 <- nonpoint_2008 + pm25$Emissions[i]
    }
  } else if(pm25$type[i]=="ON-ROAD"){
    if(pm25$year[i] == 1999 && pm25$fips[i] == 24510){
      onroad_1999 <- onroad_1999 + pm25$Emissions[i]
    } 
    else if (pm25$year[i] == 2002 && pm25$fips[i] == 24510){
      onroad_2002 <- onroad_2002 + pm25$Emissions[i]
    }
    else if (pm25$year[i] == 2005 && pm25$fips[i] == 24510){
      onroad_2005 <- onroad_2005 + pm25$Emissions[i]
    } else if (pm25$year[i] == 2008 && pm25$fips[i] == 24510) {
      onroad_2008 <- onroad_2008 + pm25$Emissions[i]
    }
  } else if(pm25$type[i]=="NON-ROAD"){
    if(pm25$year[i] == 1999 && pm25$fips[i] == 24510){
      nonroad_1999 <- nonroad_1999 + pm25$Emissions[i]
    } 
    else if (pm25$year[i] == 2002 && pm25$fips[i] == 24510){
      nonroad_2002 <- nonroad_2002 + pm25$Emissions[i]
    }
    else if (pm25$year[i] == 2005 && pm25$fips[i] == 24510){
      nonroad_2005 <- nonroad_2005 + pm25$Emissions[i]
    } else if (pm25$year[i] == 2008 && pm25$fips[i] == 24510) {
      nonroad_2008 <- nonroad_2008 + pm25$Emissions[i]
    }
  }
  cat("on ", toString(i) ,"\n")
}

pm25_trend <- data.frame(matrix(vector(), 4, 5, dimnames=list(c(), c("year", "point", "nonpoint", "onroad", "nonroad"))))

pm25_trend$year <- (c(1999, 2002, 2005, 2008))

pm25_trend$point <- (c(point_1999, point_2002, point_2005, point_2008))

pm25_trend$nonpoint <- (c(nonpoint_1999, nonpoint_2002, nonpoint_2005, nonpoint_2008))

pm25_trend$onroad <- (c(onroad_1999, onroad_2002, onroad_2005, onroad_2008))

pm25_trend$nonroad <- (c(nonroad_1999, nonroad_2002, nonroad_2005, nonroad_2008))

plot3 <- ggplot() +  
  geom_line(data=pm25_trend, aes(year, point, colour = "Point"), size = 0.5)+
  geom_line(data=pm25_trend, aes(year, nonpoint, colour = "Non-Point"), size = 0.5)+
  geom_line(data=pm25_trend, aes(year, onroad, colour = "On-Road"), size = 0.5)+
  geom_line(data=pm25_trend, aes(year, nonroad, colour = "Non-Road"), size = 0.5)+
  scale_colour_manual(values = c(
    'Point' = 'black',
    'Non-Point' = 'red',
    'On-Road' = 'purple',
    'Non-Road' = 'blue'
  ))+  
  scale_y_continuous(name = expression("PM2.5 Emissions (tons)"), limits = c(0, 2500))+
  labs(y = "PM2.5 Emissions (tons)",
       x = "Year",
       colour = 'Legend') +
  ggtitle("Annual PM2.5 Emissions from 1999 to 2008","Baltimore City") +
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

plot3

dev.copy(png, file="DataScience/exploratory_data_analysis/week4/plot3.png", height=480, width=480)
dev.off()
