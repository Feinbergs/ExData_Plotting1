library(dplyr)

plot2 <- function() {
  #read in text file. Define decimal point and delimiter char.
  #define NA char and column classes
  power <- read.table("household_power_consumption.txt"
                      , header = TRUE, dec="."
                      , sep = ";"
                      , na.strings="?"
                      , nrows =  2075259
                      , colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
                      )
  power <- filter(power, Date == "1/2/2007" | Date == "2/2/2007")
  power <- mutate(power, DateTime = paste(Date, Time))
  power$DateTime <- as.POSIXct(strptime(power$DateTime, format = '%d/%m/%Y %H:%M:%S',tz = ""))
  
  #scatter plot with x variable, y variable
  #line type
  with(power, plot(DateTime, Global_active_power, type= "l")) 
  dev.copy(png, file="plot2.png", width = 480, height = 480)
  dev.off() 
}