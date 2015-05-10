plot1 <- function(path = "./household_power_consumption.txt") {
    library(dplyr)
    
    tab5rows <- read.table(path, header = TRUE, nrows = 5, sep = ";")
    classes <- sapply(tab5rows, class)
    
    data <- read.table(path, header = TRUE, sep = ";", na.strings = "?",
                       colClasses = classes) %>%
        transform(., Date = as.Date(Date, "%d/%m/%Y")) %>% 
        filter(., Date == as.Date("1/2/2007", "%d/%m/%Y") | 
                       Date == as.Date("2/2/2007", "%d/%m/%Y"))

    png(filename = "plot1.png", width = 480, height = 480, units = "px")
    hist(data$Global_active_power, breaks = 12, main = "Global Active Power", col = "red", 
         xlab = "Global Active Power (kilowatts)", ylab = "Frequency")
    dev.off()
}