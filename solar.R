# uncomment for first run only
#install.packages("reshape")
#install.packages("ggplot2")
#install.packages("lubridate")
library(reshape)
library(ggplot2)
library(lubridate)

# pivots the table so that years are in columns
pivotByYear <- function(irradianceTable, valueColumnName) {
	dataWithYearsInColumns <- cast(irradianceTable, HOUR_IN_YEAR + MONTH + HOUR_IN_DAY ~ YEAR, value=valueColumnName)
}


# average irradiance for each hour in year
averageByHourInYear <- function(irradiance) {

	avgsByHourInYear <- aggregate(
	cbind(DIFFUSED,DIRECT) ~ HOUR_IN_YEAR + MONTH + HOUR_IN_DAY,
	data = irradiance, FUN = mean)
	# avgsByHourInYear <- aggregate(
	# irradiance[,c("DIFFUSED","DIRECT"), drop=FALSE],
	# by = list(HOUR_IN_YEAR=irradiance$HOUR_IN_YEAR, MONTH=irradiance$MONTH, HOUR_IN_DAY=irradiance$HOUR_IN_DAY),
	# FUN = mean, na.rm = TRUE)

	avgsByHourInYear <- avgsByHourInYear[ order(avgsByHourInYear$HOUR_IN_YEAR), ]
}


# average irradiance for each hour of the day in month
averageByHourOfDayAndMonth <- function(irradiance) {

	avgsByHourOfDayAndMonth <- aggregate(
	cbind(DIFFUSED,DIRECT) ~ MONTH + HOUR_IN_DAY, data = irradiance, FUN = mean)
	# avgsByHourOfDayAndMonth <- aggregate(
	# irradiance[,c("DIFFUSED","DIRECT"), drop=FALSE],
	# by = list(MONTH=irradiance$MONTH, HOUR_IN_DAY=irradiance$HOUR_IN_DAY),
	# FUN = mean, na.rm = TRUE)

	avgsByHourOfDayAndMonth <- avgsByHourOfDayAndMonth[ order(avgsByHourOfDayAndMonth$MONTH, avgsByHourOfDayAndMonth$HOUR_IN_DAY), ]
}


# total irradiance for each month
totalByMonth <- function(irradiance) {

	sumByMonth <- aggregate(cbind(DIFFUSED,DIRECT) ~ MONTH, data = irradiance, FUN = sum)
	# sumByMonth <- aggregate(
	# irradiance[,c("DIFFUSED","DIRECT"), drop=FALSE],
	# by = list(MONTH=irradiance$MONTH),
	# FUN = sum, na.rm = TRUE)

	sumByMonth <- sumByMonth[ order(sumByMonth$MONTH), ]
}


extractIrradianceData <- function(inData) {
	dateTimes <- as.POSIXlt(strptime(inData$MESS_DATUM_WOZ, format='%Y%m%d%H:%M')) # 2016063023:00

	irradiance <- data.frame(
	YEAR = 1900 + dateTimes$year,
	MONTH = dateTimes$mon +1,      # mon is 0-based
	yday = dateTimes$yday,         # yday is 0-based
	mday = dateTimes$mday,         # mday is 1-based
	HOUR_IN_DAY = dateTimes$hour,  # hour is 0-based
	DIFFUSED = as.numeric(inData$DIFFUS_HIMMEL_KW_J),
	GLOBAL = as.numeric(inData$GLOBAL_KW_J)
	)

	irradiance$DIRECT <- irradiance$GLOBAL - irradiance$DIFFUSED

	# filter out 29th of Feb for leap years
	irradiance <- subset(irradiance, !(MONTH == 2 & mday == 29))

	# shift yday by 1 after removing 29th of Feb
	years <- unique(dateTimes$year)
	leapYears <- 1900 + years[leap_year(years)]
	irradiance$yday <- irradiance$yday - (irradiance$YEAR %in% leapYears & irradiance$MONTH > 2)

	irradiance$HOUR_IN_YEAR <- irradiance$yday*24 + irradiance$HOUR_IN_DAY+1   # yday is 0-based

	result <- subset(irradiance, select=-c(mday, yday))
}


extractOtherData <- function(inData) {
	dateTimes <- as.POSIXlt(strptime(inData$MESS_DATUM_WOZ, format='%Y%m%d%H:%M')) # 2016063023:00
	retult <- data.frame(
	STATION = unique(inData$STATIONS_ID),
	FIRST_DATE = as.Date(strptime(head(inData$MESS_DATUM_WOZ,n=1), format='%Y%m%d%H:%M')),
	LAST_DATE = as.Date(strptime(tail(inData$MESS_DATUM_WOZ,n=1), format='%Y%m%d%H:%M'))
	)
}


readSingleFile <- function(fileName, colClasses = NA) {
	inData <- read.csv(fileName, header=TRUE, dec=".", sep=";", strip.white=TRUE, na.strings=c("-999", "?"), comment.char="#", colClasses = colClasses)
}

#-----------------

calculateIrradiance <- function(path, plotData) {
	# unzip all files
	#files <- list.files(path=paste(path,'database', sep="/"), pattern=".*\\.zip$", full.names=TRUE, recursive=FALSE)
	#for (fileName in files) {
	#	unzip(fileName, exdir = "./unzip")
	#}

	# list all data files
	# files <- list.files(path=paste(path,'unzip', sep="/"), pattern="produkt_.*\\.txt$", full.names=TRUE, recursive=FALSE)
	files <- list.files(path=paste(path,'unzip', sep="/"), pattern="produkt_.*03987\\.txt$", full.names=TRUE, recursive=FALSE)

	for (fileName in files) {

		inData <- readSingleFile(fileName)
		irradiance <- extractIrradianceData(inData)
		otherData <- extractOtherData(inData)

		dataInterval <- sprintf("(%s to %s)", format(otherData$FIRST_DATE), format(otherData$LAST_DATE))
		outputDir <- paste(".", "output", paste(otherData$STATION, dataInterval), sep="/")
		dir.create(outputDir, recursive = TRUE)

		# average irradiance for each hour in year
		#		avgsByHourInYear <- averageByHourInYear(irradiance)
		#		avgsByDayInYear <- averageByDayInYear(irradiance)

		# average irradiance for each hour of the day in month
		avgsByHourOfDayAndMonth <- averageByHourOfDayAndMonth(irradiance)
		#aprilData <- subset(avgsByHourOfDayAndMonth, MONTH=4)
		if (plotData) {
			p <- ggplot(avgsByHourOfDayAndMonth, aes(x=HOUR_IN_DAY)) +

			geom_line(aes(y=DIFFUSED, colour="diffused")) +
			geom_point(aes(y=DIFFUSED, colour="diffused")) +

			geom_line(aes(y=DIRECT, colour="direct")) +
			geom_point(aes(y=DIRECT, colour="direct")) +

			facet_wrap(~MONTH) +

			ggtitle(sprintf("Avg by hour of the day in each month %s", dataInterval)) +
			xlab("Hour") + ylab("Irradiance")

			png(paste(outputDir, 'avgsByHourOfDayAndMonth.png', sep="/"), width=1200, height=800, res=120)
			print(p)
			dev.off()
		}

		# total irradiance for each month
		sumByMonth <- totalByMonth(irradiance)
		if (plotData) {
			p <- ggplot(sumByMonth, aes(x=MONTH)) +

			geom_line(aes(y=DIFFUSED, colour="diffused")) +
			geom_point(aes(y=DIFFUSED, colour="diffused")) +

			geom_line(aes(y=DIRECT, colour="direct")) +
			geom_point(aes(y=DIRECT, colour="direct")) +

			scale_x_discrete(limits=1:12) +
			ggtitle(sprintf("Total irradiance per month %s", dataInterval)) +
			xlab("Month") + ylab("Irradiance")

			png(paste(outputDir, 'totalByMonth.png', sep="/"), width=800, height=600, res=120)
			print(p)
			dev.off()
		}

		# print to files
		write.csv2(avgsByHourOfDayAndMonth, paste(outputDir, 'avgsByHourOfDayAndMonth.csv', sep="/"))
		write.csv2(sumByMonth, paste(outputDir, 'totalByMonth.csv', sep="/"))
		write.csv2(otherData, paste(outputDir, 'otherData.csv', sep="/"))
	}
}