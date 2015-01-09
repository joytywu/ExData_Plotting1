plot3 <- function(){
	
	#Reading in the household consumption data
	data <- read.csv("household_power_consumption.txt", sep=";", colClasses = c("character", "character", "numeric", 	"numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings = "?")
	
	#Create a new column "DateTime" which combines column 1 and 2
	data$DateTime <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
	
	#Include only the data from "1/2/2007" to "2/2/2007" and assign a new name "includeData"
	includeData <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007",]
	
	#Exclude the NA values to plot Plot3
	plot3data <- includeData[complete.cases(includeData[, 7:9]),]
	
	#Set parameters
	par(mar = c(5, 4, 4, 3))
	
	#Set x and y for the plot
	x <- plot3data$DateTime
	y <- plot3data[, 7:9]
	
	#Plot3 with just sub_metering_1 first
	plot3 <- plot(x, y[, 1], ylab = "Energy sub metering", xlab = " ", type = "l")
	
	#Add the data for meter 2 and 3 into the plot
	points(x, y[, 2], col = "red", type = "l")
	points(x, y[, 3], col = "blue", type = "l")
	
	#Add legend
	legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1,1), col = c("black", "red", "blue"))
	
	#Store as PNG file
	dev.copy(png, file = "plot3.png")
	dev.off()
	
	print(plot3)


	
}	