plot2 <- function()
{
  #Library lubridate needed
  rawdata <- read.table("household_power_consumption.txt",header=T,sep=";",na.strings="?",colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"));
  data <- rawdata[(rawdata$Date=="1/2/2007" | rawdata$Date=="2/2/2007"),]
  
  timeVector <- vector();
  powerVector <- vector();
  for(i in 1:nrow(data))
  {

    time <- hms(data[i,"Time"]);
    timeInMinutes <- hour(time)*60 + minute(time);
    
    #If it is 2nd Feb then add 24 hours
    if(data[i,"Date"]=="2/2/2007")
    {
      timeInMinutes <- timeInMinutes + 24*60;
    }
    timeVector <- c(timeVector,timeInMinutes)
    powerVector <- c(powerVector,data[i,"Global_active_power"])
    
  }
  
  plot(timeVector,powerVector, type='n',xaxt='n',xlab="",ylab="Global Active Power(kilowatts)",main="")
  lines(timeVector,powerVector)
  axis(1,at = c(0,24*60,48*60),c("Thu","Fri","Sat"))
  
  dev.copy(png,file="plot2.png")
  dev.off()
  
}