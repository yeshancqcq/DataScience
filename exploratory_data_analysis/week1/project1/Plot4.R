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

# making plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(1,1,1,1))
with(chosen, {
  # 1st plot
  plot(Global_active_power~weekdays, type="l", ylab="Global Active Power (kilowatts)", col="#de9b95", xlab="")
  # 2nd plot
  plot(Voltage~weekdays, type="l", ylab="Voltage", xlab="datetime", col="black")
  # 3rd plot
  plot(Sub_metering_1~weekdays, type="l", ylab="Global Active Power (kilowatts)", col="black", xlab="")
  # Adding 2 more lines to 3rd plot
  lines(Sub_metering_2~weekdays,col='blue')
  lines(Sub_metering_3~weekdays,col='red')
  legend("topright", col=c("black", "blue", "red"), lwd=1, bty="l",
         legend=c("sub_Metering_1","sub_Metering_2","sub_Metering_3"), cex = 0.65)
  # 4th plot
  plot(Global_reactive_power~weekdays, type="l", ylab="Global Rective Power (kilowatts)", col = "black", xlab="datetime")
})

dev.copy(png, file="./project1/Plot4.png", height=480, width=480)
dev.off()
