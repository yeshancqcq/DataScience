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

# making plot 2
plot(chosen$Global_active_power~chosen$weekdays,  main="Global Active Power Thu to Sat", type="l",ylab="Global Active Power (kW)", xlab="", col = "#de9b95")

# making plot 3
with(chosen, {plot(Sub_metering_1~weekdays, type="l", main = "Energy Sub-Metering", col="#de9b95",ylab="Global Active Power (kW)", xlab="")
                    #Adding two more lines to the same graph
                    lines(Sub_metering_2~weekdays,col='black')
                    lines(Sub_metering_3~weekdays,col='#95b0de')
})
legend("topright", col=c("#de9b95", "black", "#95b0de"), lwd=1, legend=c("Kitchen", "Laundry Room", "Heater and AC"))

# making plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(1,1,1,1))
with(chosen, {
  # 1st plot
  plot(Global_active_power~weekdays, type="l", ylab="Global Active Power (kW)", col="#de9b95", xlab="")
  # 2nd plot
  plot(Voltage~weekdays, type="l", ylab="Voltage (V)", xlab="datetime", col="#95b0de")
  # 3rd plot
  plot(Sub_metering_1~weekdays, type="l", ylab="Global Active Power (kW)", col="#de9b95", xlab="")
  # Adding 2 more lines to 3rd plot
  lines(Sub_metering_2~weekdays,col='black')
  lines(Sub_metering_3~weekdays,col='#95b0de')
  legend("topright", col=c("#de9b95", "black", "#95b0de"), lwd=1, bty="l",
         legend=c("Kitchen", "Laundry Room", "Heater and AC"), cex = 0.65)
  # 4th plot
  plot(Global_reactive_power~weekdays, type="l", ylab="Global Rective Power (kW)", col = "#a7c4bb", xlab="datetime")
})
