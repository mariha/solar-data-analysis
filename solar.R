#install.packages("reshape")
#install.packages("ggplot2")
library(reshape)
library(ggplot2)

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
    HOUR_IN_YEAR = dateTimes$yday*24 + dateTimes$hour+1,
    MONTH = dateTimes$mon,
    HOUR_IN_DAY = dateTimes$hour,
    YEAR = 1900 + dateTimes$year,
    DIFFUSED = as.numeric(inData$DIFFUS_HIMMEL_KW_J),
    GLOBAL = as.numeric(inData$GLOBAL_KW_J)
    )

    irradiance$DIRECT <- irradiance$GLOBAL - irradiance$DIFFUSED

    # filter out 29th of Feb for leap years
    irradiance <- subset(irradiance, HOUR_IN_YEAR <= 365*24)

    result <- irradiance
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
    files <- list.files(path=paste(path,'database', sep="/"), pattern=".*\\.zip$", full.names=TRUE, recursive=FALSE)
    for (fileName in files) {
        unzip(fileName, exdir = "./unzip")
    }

    # list all data files
    files <- list.files(path=paste(path,'unzip', sep="/"), pattern="produkt_.*\\.txt$", full.names=TRUE, recursive=FALSE)
    print(files)

    for (fileName in files) {
        #unzip(fileName, exdir = "./database/test")

        inData <- readSingleFile(fileName)
        irradiance <- extractIrradianceData(inData)
        otherData <- extractOtherData(inData)

        outputDir <- paste(".", "output", otherData$STATION, sep="/")
        dir.create(outputDir, recursive = TRUE)

        # average irradiance for each hour in year
        avgsByHourInYear <- averageByHourInYear(irradiance)
        #		avgsByDayInYear <- averageBydayInYear(irradiance)

        # average irradiance for each hour of the day in month
        avgsByHourOfDayAndMonth <- averageByHourOfDayAndMonth(irradiance)
        #aprilData <- subset(avgsByHourOfDayAndMonth, MONTH=4)
        if (plotData) {
            p <- ggplot(avgsByHourOfDayAndMonth) +
            geom_ribbon(aes(x=HOUR_IN_DAY, ymin=0, ymax=DIFFUSED), fill='orange') +
            geom_ribbon(aes(x=HOUR_IN_DAY, ymin=DIFFUSED, ymax=DIRECT+DIFFUSED), fill='yellow') +
            facet_wrap(~MONTH) +
            ggtitle("Avg by hour of the day in each month") +
            xlab("Hour") + ylab("Irradiance")

            png(paste(outputDir, 'avgsByHourOfDayAndMonth.png', sep="/"), res=120)
            print(p)
            dev.off()
        }

        # total irradiance for each month
        sumByMonth <- totalByMonth(irradiance)
        if (plotData) {
            p <- ggplot(sumByMonth) +
            geom_ribbon(aes(x=MONTH, ymin=0, ymax=DIFFUSED), fill='orange') +
            geom_ribbon(aes(x=MONTH, ymin=DIFFUSED, ymax=DIRECT+DIFFUSED), fill='yellow') +
            ggtitle("Total irradiance per month") +
            xlab("Month") + ylab("Irradiance")

            png(paste(outputDir, 'totalByMonth.png', sep="/"), res=120)
            print(p)
            dev.off()
        }

        # print to files
        write.csv2(avgsByHourOfDayAndMonth, paste(outputDir, 'avgsByHourOfDayAndMonth.csv', sep="/"))
        write.csv2(sumByMonth, paste(outputDir, 'totalByMonth.csv', sep="/"))
        write.csv2(otherData, paste(outputDir, 'otherData.csv', sep="/"))
    }
}