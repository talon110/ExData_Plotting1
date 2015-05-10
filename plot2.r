plot2 <- function(path = "./household_power_consumption.txt") {
    library(dplyr)
    
    tab5rows <- read.table(path, header = TRUE, nrows = 5, sep = ";", stringsAsFactors = F)
    classes <- sapply(tab5rows, class)
    
    data <- read.table(path, header = TRUE, sep = ";", na.strings = "?",
                       colClasses = classes, stringsAsFactors = F) %>%
        filter(., Date == "1/2/2007" | Date == "2/2/2007") %>%
        select(., Date:Sub_metering_3) %>%
        transform(., Date = strptime(paste(Date, Time, sep = " "), "%d/%m/%Y %H:%M:%S")) %>%
        select(., -(Time))
    
    png(filename = "plot2.png", width = 480, height = 480, units = "px")
    plot(data$Date, data$Global_active_power, type = "l", xlab = "",
         ylab = "Global Active Power (kilowatts)")
    dev.off()
}