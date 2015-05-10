plot4 <- function(path = "./household_power_consumption.txt") {
    library(dplyr)
    
    tab5rows <- read.table(path, header = TRUE, nrows = 5, sep = ";", stringsAsFactors = F)
    classes <- sapply(tab5rows, class)
    
    data <- read.table(path, header = TRUE, sep = ";", na.strings = "?",
                       colClasses = classes, stringsAsFactors = F) %>%
        filter(., Date == "1/2/2007" | Date == "2/2/2007") %>%
        select(., Date:Sub_metering_3) %>%
        transform(., Date = strptime(paste(Date, Time, sep = " "), "%d/%m/%Y %H:%M:%S")) %>%
        select(., -(Time))
    
    png(filename = "plot4.png", width = 480, height = 480, units = "px")
    par(mfrow=c(2,2))
    
    # top left plot (global active power per day)
    with(data, plot(Date, Global_active_power, type = "l", xlab = "",
         ylab = "Global Active Power (kilowatts)"))
    
    # top right plot (voltage per day)
    with(data, plot(Date, Voltage, type = "l", xlab = "datetime",
                    ylab = "Voltage"))
    
    # bottom left plot (energy sub metering)
    with(data, {
        plot(Date, Sub_metering_1, col = "black", type = "l", ylab = "Engergy sub metering", 
             xlab = "")
        lines(Date, Sub_metering_2, col = "red", type = "l")
        lines(Date, Sub_metering_3, col = "blue", type = "l")
    })
    legend("topright", col = c("black", "red", "blue"), lty = "solid", 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    # bottom right plot (global reactive power per day)
    with(data, plot(Date, Global_reactive_power, type = "l", xlab = "datetime",
                    ylab = "Global Reactive Power"))
    dev.off()
}