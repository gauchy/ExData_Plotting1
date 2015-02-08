plot4 <- function()
{
  #Library lubridate needed
  rawdata <- read.table("household_power_consumption.txt",header=T,sep=";",na.strings="?",colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"));
  data <- rawdata[(rawdata$Date=="1/2/2007" | rawdata$Date=="2/2/2007"),]
  
  timeVector <- vector();
  submetering1 <- vector();
  submetering2 <- vector();
  submetering3 <- vector();
  powerVector <- vector();
  reactivePowerVector <- vector();
  voltage <- vector();
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
    submetering1 <- c(submetering1,data[i,"Sub_metering_1"])
    submetering2 <- c(submetering2,data[i,"Sub_metering_2"])
    submetering3 <- c(submetering3,data[i,"Sub_metering_3"])
    powerVector <- c(powerVector,data[i,"Global_active_power"])
    voltage <- c(voltage,data[i,"Voltage"])
    reactivePowerVector <- c(reactivePowerVector,data[i,"Global_reactive_power"])
  }
  
  par(mfcol = c(2,2))
  
  plot(timeVector,powerVector, type='n',xaxt='n',xlab="",ylab="Global Active Power(kilowatts)",main="")
  lines(timeVector,powerVector)
  axis(1,at = c(0,24*60,48*60),c("Thu","Fri","Sat"))
  
  
  plot(timeVector,submetering1, type='n',xaxt='n',xlab="",ylab="Energy sub metering",main="")
  lines(timeVector,submetering1)
  lines(timeVector,submetering2,col="red")
  lines(timeVector,submetering3,col="blue")
  axis(1,at = c(0,24*60,48*60),c("Thu","Fri","Sat"))
  legend("topright",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lwd=c(1,1,1),bty="n",seg.len=1,x.intersp=0.5)
  
  
  plot(timeVector,voltage, type='n',xaxt='n',xlab="datetime",ylab="Voltage",main="")
  lines(timeVector,voltage)
  axis(1,at = c(0,24*60,48*60),c("Thu","Fri","Sat"))
  
  plot(timeVector,reactivePowerVector, type='n',xaxt='n',xlab="datetime",ylab="Global_reactive_power",main="")
  lines(timeVector,reactivePowerVector)
  axis(1,at = c(0,24*60,48*60),c("Thu","Fri","Sat"))
  
  
  
  dev.copy(png,file="plot4.png",width=812,height=617)
  dev.off()

}