plot1 <- function()
{
  rawdata <- read.table("household_power_consumption.txt",header=T,sep=";",na.strings="?",colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"));
  data <- rawdata[(rawdata$Date=="1/2/2007" | rawdata$Date=="2/2/2007"),]
  hist(data$Global_active_power,col="red",xlab="Global Active Power(kilowatts)",main="Global Active Power")
  dev.copy(png,file="plot1.png")
  dev.off()

}