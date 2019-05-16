# Exploratory_Data_Analysis_project1

#Upload dataset
data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?")
summary(data)
str(data)

## Format date to Type Date
data$Date <- as.Date(data$Date, "%d/%m/%Y")
str(data)

# Subset data of 2 day period in February, 2007
data1 <- subset(data,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
summary(data1)

#Remove incomplete observations
data1 <- data1[complete.cases(data1),]
summary(data1)
str(data1)

## Create the histogram
hist(data$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")



#Combine date and time column
dateTime <- paste(data1$Date, data1$Time)
## Name the vector
dateTime <- setNames(dateTime, "DateTime")

## Format dateTime Column
data1$dateTime <- as.POSIXct(dateTime)

#Plot 2
plot(data1$Global_active_power~data1$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

#Plot 3
summary(data1)
with(data1, {
        plot(Sub_metering_1 ~ dateTime, type = "l", 
             ylab = "Global Active Power (kilowatts)", xlab = "")
        lines(Sub_metering_2 ~ dateTime, col = 'Red')
        lines(Sub_metering_3 ~ dateTime, col = 'Blue')
})


legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#Plot 4 
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(data1, {
        plot(Global_active_power~dateTime, type="l", 
             ylab="Global Active Power (kilowatts)", xlab="")
        plot(Voltage~dateTime, type="l", 
             ylab="Voltage (volt)", xlab="")
        plot(Sub_metering_1~dateTime, type="l", 
             ylab="Global Active Power (kilowatts)", xlab="")
        lines(Sub_metering_2~dateTime,col='Red')
        lines(Sub_metering_3~dateTime,col='Blue')
        legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        plot(Global_reactive_power~dateTime, type="l", 
             ylab="Global Rective Power (kilowatts)",xlab="")
})
