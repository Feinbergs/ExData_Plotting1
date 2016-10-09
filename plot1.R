library(dplyr)

plot1 <- function() {
  
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
  
  #define x variable for graphing
  #define spread for x axis
  x<-power$Global_active_power
  x <- (ceiling(x*2)-.25)/2
  
  barplot(table(x), main = 'Global Active Power'
          , xlab = "Global Active Power (kilowatts)"
          , ylab = "Frequency"
          , space = c(0)
          , xlim=c(0,6)
          , width=c(.5)
          , col=c("red")
          , horiz = FALSE
          , axes = FALSE
          , axisnames = FALSE)
  
  axis(side=1, at = seq(0, 6, by = 2), pos= -65)
  axis(side = 2)
  
  dev.copy(png, file="plot1.png", width = 480, height = 480)
  dev.off() 
}