plot2 <- function(){
	
	#Reading in the household consumption data
	data <- read.csv("household_power_consumption.txt", sep=";", colClasses = c("character", "character", "numeric", 	"numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.strings = "?")
	
	#Create a new column "DateTime" which combines column 1 and 2
	data$DateTime <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
	
	#Include only the data from "1/2/2007" to "2/2/2007" and assign a new name "includeData"
	includeData <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007",]
	
	#Exclude the NA values and select column 3 to plot Plot2
	plot2data <- includeData[complete.cases(includeData[, 3]),]
	
	#Plot2
	par(mar = c(5, 4, 4, 3))
	plot2 <- plot(plot2data$DateTime, plot2data$Global_active_power, ylab = "Global Active Power (kilowatts)", xlab = " ", type = "l")
	
	#Store as PNG file
	dev.copy(png, file = "plot2.png")
	dev.off()
	
	print(plot2)


	
}	