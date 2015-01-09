plot1 <- function(){
	
	#Reading in the household consumption data
	data <- read.csv("household_power_consumption.txt", sep=";", colClasses = c("character", "character", "numeric", 	"numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings = "?")
	
	#Create a new column "DateTime" which combines column 1 and 2
	data$DateTime <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
	
	#Include only the data from "1/2/2007" to "2/2/2007" and assign a new name "includeData"
	includeData <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007",]
	
	#Exclude the NA values and select column 3 to plot Plot1
	plot1data <- includeData[complete.cases(includeData[, 3]), 3]
	
	#Set margins of Plot1
	par(mar = c(5, 4, 4, 3))
	
	#Plot1
	plot1 <- hist(plot1data, col = "red", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power")
	
	#Store as PNG file
	dev.copy(png, file = "plot1.png")
	dev.off()
	
	print(plot1)
}