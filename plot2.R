## Load this file using:
##    source("plot2.R")
## Then execute:
##    plot2()
plot2 <- function() {
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

	# The required graph is a histogram about the 'Global_active_power' values
	# But the format of the data of this column is a Factor, not a number
	# For this reason, we must to convert the Factor values to numeric format
	if (is.factor(subData$Global_active_power)) {
		subData$Global_active_power <- as.numeric(levels(subData$Global_active_power)[subData$Global_active_power])
	}

	# Set a png bitmap as graphics device
	print("Plotting...")
	png(filename = "plot2.png", width = 480, height = 480)

	# Plot the graph
	# Note: Changes on x-axis was note taken effect in my computer. 
	#       My result was the day names in Spanish
	plot(subData$Time, subData$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)", axes = TRUE)
	axis(side = 1, at = 1:3, labels = c("Thu", "Fri", "Sat"))

	# Close the graphic device. This generates the png file with the graph
	dev.off()

	# Release 'subData' object from memory
	remove(subData)

	print("The file 'plot2.png' has been created.")
}