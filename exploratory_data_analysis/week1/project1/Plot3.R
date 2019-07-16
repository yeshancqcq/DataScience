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

# making plot 3
with(chosen, {plot(Sub_metering_1~weekdays, type="l", main = "Energy Sub-Metering", col="black",ylab="Global Active Power (kilowatts)", xlab="")
  #Adding two more lines to the same graph
  lines(Sub_metering_2~weekdays,col='blue')
  lines(Sub_metering_3~weekdays,col='red')
})
legend("topright", col=c("black", "blue", "red"), lwd=1, legend=c("sub_Metering_1","sub_Metering_2","sub_Metering_3"))

dev.copy(png, file="./project1/Plot3.png", height=480, width=480)
dev.off()
