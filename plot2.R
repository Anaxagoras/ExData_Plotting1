makePlot2 <- function() {
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
    
    png(filename="plot2.png", width=480, height=480)
    plot(consLimit$Time, consLimit$Global_active_power,
         type="n",
         xlab="",
         ylab="Global Active Power (kilowatts)")
    lines(consLimit$Time, consLimit$Global_active_power)
    dev.off()
}