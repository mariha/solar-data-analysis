# uncomment for first run only
#install.packages("reshape")
#install.packages("ggplot2")
#install.packages("lubridate")
library(reshape)
library(ggplot2)
library(lubridate)

#------------------------------------------------------------------------------

INPUT_DIR <- "database"
UNZIP_DIR <- "unzip"
OUTPUT_DIR <- "output"

#------------------------------------------------------------------------------

# pivots the table so that years are in columns
pivotByYear <- function(irradianceTable, valueColumnName) {
	dataWithYearsInColumns <- cast(irradianceTable, HOUR_IN_YEAR + MONTH + HOUR_IN_DAY ~ YEAR, value=valueColumnName)
}


# average irradiance for each hour in year
averageByHourInYear <- function(irradiance, includeMonth = TRUE) {

	if (includeMonth) {
		avgsByHourInYear <- aggregate(
					cbind(DIFFUSED, DIRECT, GLOBAL) ~ HOUR_IN_YEAR + MONTH + HOUR_IN_DAY,
					data = irradiance, FUN = mean)
	} else {
		avgsByHourInYear <- aggregate(
					cbind(DIFFUSED, DIRECT, GLOBAL) ~ HOUR_IN_YEAR + HOUR_IN_DAY,
					data = irradiance, FUN = mean)
	}

	avgsByHourInYear <- avgsByHourInYear[ order(avgsByHourInYear$HOUR_IN_YEAR), ]
}


# average irradiance for each hour of the day in month
averageByHourOfDayAndMonth <- function(irradiance) {

	avgsByHourOfDayAndMonth <- aggregate(
				cbind(DIFFUSED,DIRECT) ~ MONTH + HOUR_IN_DAY, data = irradiance, FUN = mean)

	avgsByHourOfDayAndMonth <- avgsByHourOfDayAndMonth[ order(avgsByHourOfDayAndMonth$MONTH, avgsByHourOfDayAndMonth$HOUR_IN_DAY), ]
}


# total irradiance for each month
totalByMonth <- function(irradiance) {

	irradiance <- averageByHourOfDayAndMonth(irradiance)

	sumByMonth <- aggregate(cbind(DIFFUSED,DIRECT) ~ MONTH, data = irradiance, FUN = sum)

	sumByMonth <- sumByMonth[ order(sumByMonth$MONTH), ]
}


# total irradiance for each month in each year
totalByMonthAndYear <- function(irradiance) {

	# sum by month and year
	sumByMonthAndYear <- aggregate(cbind(DIFFUSED,DIRECT) ~ MONTH + YEAR, data = irradiance, FUN = sum)

	# transform DIFFUSED and DIRECT columns into IRRADIANCE [type] and VALUE columns
	sumByMonthAndYear <- melt(sumByMonthAndYear, measure.vars=c("DIFFUSED","DIRECT"), variable_name="IRRADIANCE")
}

totalByMonthInYears <- function(irradiance) {
	sumByMonthAndYear <- totalByMonthAndYear(irradiance)

	# pivot by year
	sumByMonthInYears <- cast(sumByMonthAndYear, IRRADIANCE + MONTH ~ YEAR, value="VALUE")

	# sort by month and irradiance type
	sumByMonthInYears <- sumByMonthInYears[ order(sumByMonthInYears$MONTH, sumByMonthInYears$IRRADIANCE), ]
}


extractIrradianceData <- function(inData, skip29thFeb = TRUE) {
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

	if (skip29thFeb) {
		# filter out 29th of Feb for leap years
		irradiance <- subset(irradiance, !(MONTH == 2 & mday == 29))

		# shift yday by 1 after removing 29th of Feb
		years <- 1900 + unique(dateTimes$year)
		leapYears <- years[leap_year(years)]
		irradiance$yday <- irradiance$yday - (irradiance$YEAR %in% leapYears & irradiance$MONTH > 2)
	}

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

#-----------------

readSingleFile <- function(fileName, colClasses = NA) {
	inData <- read.csv(fileName, header=TRUE, dec=".", sep=";", strip.white=TRUE, na.strings=c("-999", "?"), comment.char="#", colClasses = colClasses)
}

unzipFiles <- function(sourceDirectory, destDirectory) {
	files <- list.files(sourceDirectory, pattern=".*\\.zip$", full.names=TRUE, recursive=FALSE)
	for (fileName in files) {
		unzip(fileName, exdir = destDirectory)
	}
}

#-----------------

calculateIrradiance <- function(workingDir = ".", dataFilePattern = "produkt_.*\\.txt$", unzipData = TRUE, plotData = TRUE) {

	if (unzipData) {
		unzipFiles(file.path(workingDir,INPUT_DIR), file.path(workingDir,UNZIP_DIR))
	}

	dataFiles <- list.files(path=file.path(workingDir,UNZIP_DIR), pattern=dataFilePattern, full.names=TRUE, recursive=FALSE)

	for (dataFile in dataFiles) {

		inData <- readSingleFile(dataFile)
		irradiance <- extractIrradianceData(inData)
		otherData <- extractOtherData(inData)

		dataInterval <- sprintf("(%s to %s)", format(otherData$FIRST_DATE), format(otherData$LAST_DATE))
		outputDir <- file.path(workingDir, OUTPUT_DIR, paste(otherData$STATION, dataInterval))
		if (!dir.exists(outputDir)) {
			dir.create(outputDir, recursive = TRUE)
		}

		# average irradiance for each hour of the day in month
		avgsByHourOfDayAndMonth <- averageByHourOfDayAndMonth(irradiance)
		write.csv(avgsByHourOfDayAndMonth, file.path(outputDir, "avgsByHourOfDayAndMonth.csv"))
		if (plotData) {
			p <- ggplot(avgsByHourOfDayAndMonth, aes(x=HOUR_IN_DAY)) +

					geom_line(aes(y=DIFFUSED, colour="diffused")) +
					geom_point(aes(y=DIFFUSED, colour="diffused")) +

					geom_line(aes(y=DIRECT, colour="direct")) +
					geom_point(aes(y=DIRECT, colour="direct")) +

					facet_wrap(~MONTH) +

					ggtitle(sprintf("Avg by hour of the day in each month %s", dataInterval)) +
					xlab("Hour") + ylab("Irradiance")

			png(file.path(outputDir, "avgsByHourOfDayAndMonth.png"), width=1200, height=800, res=120)
			print(p)
			dev.off()
		}

		# total irradiance for each month
		sumByMonth <- totalByMonth(irradiance)
		write.csv(sumByMonth, file.path(outputDir, "totalByMonth.csv"))
		if (plotData) {
			p <- ggplot(sumByMonth, aes(x=MONTH)) +

					geom_line(aes(y=DIFFUSED, colour="diffused")) +
					geom_point(aes(y=DIFFUSED, colour="diffused")) +

					geom_line(aes(y=DIRECT, colour="direct")) +
					geom_point(aes(y=DIRECT, colour="direct")) +

					scale_x_discrete(limits=1:12) +
					ggtitle(sprintf("Total irradiance per month %s", dataInterval)) +
					xlab("Month") + ylab("Irradiance")

			png(file.path(outputDir, "totalByMonth.png"), width=800, height=600, res=120)
			print(p)
			dev.off()
		}

		# average irradiance for each month in each year
		sumByMonthInYears <- totalByMonthInYears(irradiance)
		write.csv(sumByMonthInYears, file.path(outputDir, "totalByMonthInYears.csv"))
		if (plotData) {
			sumByMonthAndYear <- totalByMonthAndYear(irradiance)
			# avarage in every 5 years
			#sumByMonthAndYear$YEAR <- sumByMonthAndYear$YEAR %/% 5 * 5
			#sumByMonthAndYear <- aggregate(value ~ IRRADIANCE + MONTH + YEAR, data = sumByMonthAndYear, FUN = mean)
			sumByMonthAndYear <- subset(sumByMonthAndYear, IRRADIANCE=="DIFFUSED")

			p <- ggplot(sumByMonthAndYear, aes(x=MONTH)) +

					geom_line(aes(y=value, colour=YEAR, group=YEAR)) +

					scale_x_discrete(limits=1:12) +
					ggtitle(sprintf("Total diffused irradiance per month in each year %s", dataInterval)) +
					xlab("Month") + ylab("Irradiance")

			#print(p)
			png(file.path(outputDir, "totalByMonthInYears-diffused.png"), width=800, height=600, res=120)
			print(p)
			dev.off()
		}

		# average irradiance for each hour in year
		avgsByHourInYear <- averageByHourInYear(irradiance)
		write.csv(avgsByHourInYear, file.path(outputDir, "avgsByHourOfYear-skip29thFeb.csv"))

		irradiance <- extractIrradianceData(inData, skip29thFeb = FALSE)
		avgsByHourInYear <- averageByHourInYear(irradiance, includeMonth=FALSE)
		write.csv(avgsByHourInYear, file.path(outputDir, "avgsByHourOfYear-noSkip-noMonth.csv"))
	}
}