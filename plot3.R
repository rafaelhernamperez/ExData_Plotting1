## Load this file using:
##    source("plot3.R")
## Then execute:
##    plot3()
plot3 <- function() {
	# Read the data file and load it the 'data' object
	# 'data' object will contain 2,075,250 observations in 9 variables
	print("Loading data. Please, wait for a few seconds...")
	data <- read.csv("household_power_consumption.txt", sep=";")

	# The 'Date' and 'Time' columns are originally load as text data format
	# Convert the values of the 'Time' column to POSIXlt format
	# For this operation, the 'Date' and 'Time' values are combined
	print("Coercing data...")
	data$Time <- strptime(paste(data$Date, " ", data$Time), "%d/%m/%Y %H:%M:%S")

	# Convert the values of the 'Date' column to a real Date format
	data$Date <- as.Date(data$Date, "%d/%m/%Y")

	# Initializa the start and end dates
	startDate <- as.Date("2007-02-01", "%Y-%m-%d")
	endDate <- as.Date("2007-02-02", "%Y-%m-%d")

	# Extract only the data between start and end dates, and load it into 'subData' object
	# 'subData' object will contain 2,880 observations in 9 variables
	print("Filtering data...")
	subData <- data[data$Date >= startDate & data$Date <= endDate,]

	# Remove 'data', 'startDate' and 'endDatae' objects and release memory. 
	# From this point, we will work with the 'subData' object
	print("Releasing memory...")
	remove(data)
	remove(startDate)
	remove(endDate)

	# The required graph is a linebar about the 'Sub_metering_x' values
	# But the format of the data of these columns is a Factor, not a number
	# For this reason, we must to convert the Factor values to numeric format
	if (is.factor(subData$Sub_metering_1)) {
		subData$Sub_metering_1 <- as.numeric(levels(subData$Sub_metering_1)[subData$Sub_metering_1])
	}

	if (is.factor(subData$Sub_metering_2)) {
		subData$Sub_metering_2 <- as.numeric(levels(subData$Sub_metering_2)[subData$Sub_metering_2])
	}
	
	if (is.factor(subData$Sub_metering_3)) {
		subData$Sub_metering_3 <- as.numeric(levels(subData$Sub_metering_3)[subData$Sub_metering_3])
	}

	# Set a png bitmap as graphics device
	print("Plotting...")
	png(filename = "plot3.png", width = 480, height = 480)

	# Plot the graph
	# Note: Changes on x-axis was note taken effect in my computer. 
	#       My result was the day names in Spanish
	plot(subData$Time, subData$Sub_metering_1, col = "black", type = "l", xlab = "", ylab = "Energy sub metering", axes = TRUE)
	lines(subData$Time, subData$Sub_metering_2, col = "red", type = "l")
	lines(subData$Time, subData$Sub_metering_3, col = "blue", type = "l")
	axis(side = 1, at = 1:3, labels = c("Thu", "Fri", "Sat"))

	legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = c(1, 1, 1))
	
	# Close the graphic device. This generates the png file with the graph
	dev.off()

	# Release 'subData' object from memory
	remove(subData)

	print("The file 'plot3.png' has been created.")
}