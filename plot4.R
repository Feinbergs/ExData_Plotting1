library(dplyr)

plot4 <- function() {
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
  
  
  #four graphs, 2 rows 2 columns
  par(mfrow=c(2,2))  
  #graph 1
  with(power, plot(DateTime, Global_active_power    
                   , type= "l"))                           
  #graph 2
  with(power, plot(DateTime, Voltage, pch=16))      
  
  #graph 3
  with(power, plot(DateTime, Sub_metering_1         
                   , type = "l"                            
                   , col = "black"                         
                   , xlab="DateTime"                       
                   , ylab="Energy Sub Metering"))          
  lines(power$DateTime, power$Sub_metering_2        
        , col="red")                            
  lines(power$DateTime, power$Sub_metering_3        
        , col="blue")                           
  legend("topright"                                
         , c("Sub_metering_1","Sub_metering_2","Sub_metering_3")  
         , lty = 1                                                
         , col = c("black", "red", "blue"))                       
  
  #graph 4
  with(power, plot(DateTime, Global_reactive_power  
                   , type= "l"))    
  
  dev.copy(png, file="plot4.png", width = 480, height= 480)
  dev.off() 
}