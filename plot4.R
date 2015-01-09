plot4 <- function(){
	
	#Reading in the household consumption data
	data <- read.csv("household_power_consumption.txt", sep=";", colClasses = c("character", "character", "numeric", 	"numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings = "?")
	
	#Create a new column "DateTime" which combines column 1 and 2
	data$DateTime <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
	
	#Include only the data from "1/2/2007" to "2/2/2007" and assign a new name "includeData"
	includeData <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007",]
	
	#EXCLUDE NA VALUES BEFORE PLOTTING:
	plotdata <- includeData[complete.cases(includeData[, 3:9]),]
	
	#Assigning plotting variables
	x <- plotdata$DateTime  
	Global_ap <- plotdata[, 3]
	Global_rp <- plotdata[, 4]
	voltage <- plotdata[, 5]
	submeter1 <- plotdata[, 7]
	submeter2 <- plotdata[, 8]
	submeter3 <- plotdata[, 9]
	
	
	#Set parameters
	par(mfrow = c(2,2), mar = c(4, 4, 1, 1))
	
	
	#PLOTTING: 
	
		plot(x, Global_ap, ylab = "Global Active Power", xlab = " ", type = "l")
		plot(x, voltage, xlab = "datetime", ylab = "Voltage", type = "l")
		plot(x, submeter1, ylab = "Energy Sub Metering", xlab = " ", col = "black", type = "l")
			points(x, y[, 2], col = "red", type = "l")
			points(x, y[, 3], col = "blue", type = "l")
			legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1,1), col = 			c("black", "red", "blue"))
		plot(x, Global_rp, ylab = "Global_reactive_power", xlab = "datetime", type = "l")
	
	
	#Store as PNG file
	dev.copy(png, file = "plot4.png")
	dev.off()
	
	
}	