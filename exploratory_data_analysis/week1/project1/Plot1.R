#Checking and setting working directory
getwd()
setwd("Documents/github/DataScience/exploratory_data_analysis/week1")

data <- read.csv("./data/household_power_consumption.txt", header=T, sep=';', na.strings="?", nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')

# Choosing data between Feb 1 and Feb 2
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
chosen <- subset(data, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
# Converting to weekdays
converting <- paste(as.Date(chosen$Date), chosen$Time)
chosen$weekdays <- as.POSIXct(converting)

# making plot 1
hist(chosen$Global_active_power, main="Global Active Power", xlab="Global Active Power (kW)", ylab="Frequency", col="#de9b95")

dev.copy(png, file="./project1/plot1.png", height=500, width=500)
dev.off()