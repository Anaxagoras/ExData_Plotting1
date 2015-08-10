makePlot3 <- function() {
    consUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(consUrl, "household_power_consumption.zip", method = "curl")
    
    consumption <- read.table(
        unz("household_power_consumption.zip", "household_power_consumption.txt"),
        header = TRUE,
        sep = ";")
    
    consumption$Time <- paste(consumption$Date, consumption$Time)
    consumption$Date <- as.Date(levels(consumption$Date), format = "%d/%m/%Y")[consumption$Date]
    consLimit <- subset(consumption,
                        (Date >= as.Date("2007-02-01", "%Y-%m-%d")) 
                        & (Date <= as.Date("2007-02-02", "%Y-%m-%d")))
    consLimit$Global_active_power <- as.numeric(levels(consLimit$Global_active_power))[consLimit$Global_active_power]
    consLimit$Time <- strptime(consLimit$Time, "%d/%m/%Y %H:%M:%S")
    consLimit$Sub_metering_1 <- as.numeric(levels(consLimit$Sub_metering_1))[consLimit$Sub_metering_1]
    consLimit$Sub_metering_2 <- as.numeric(levels(consLimit$Sub_metering_2))[consLimit$Sub_metering_2]
    consLimit$Sub_metering_3 <- as.numeric(levels(consLimit$Sub_metering_3))[consLimit$Sub_metering_3]
    
    png(filename="plot3.png", width=480, height=480)
    plot(consLimit$Time, consLimit$Sub_metering_1,
         type="n",
         xlab="",
         ylab="Energy sub metering")
    lines(consLimit$Time, consLimit$Sub_metering_1, col="black")
    lines(consLimit$Time, consLimit$Sub_metering_2, col="red")
    lines(consLimit$Time, consLimit$Sub_metering_3, col="blue")
    legend("topright", lty=1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    dev.off()
}