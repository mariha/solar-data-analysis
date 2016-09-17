source("solar.R")

compare <- function(testName, actual, expected){
	result <- all.equal(expected, actual, tolerance=0.01, check.attributes=FALSE, check.names=TRUE)
	if (!isTRUE(result)){
		print("")
		print(result)
		print(paste("TEST NAME: ", testName))

		print("expected:")
		print(expected)
		print("actual:")
		print(actual)

		print("expected:")
		str(expected)
		print("actual:")
		str(actual)
		result <- testName
	}
}

#------------------------------------------------------------------------------

results <- list()
i <- 0L

#------------------------------------------------------------------------------

# TEST: compare function

	inData <- readSingleFile("tests/sample.in.txt")
	actualOutData <- inData
	expectedOutData <- inData
	# make test failure
	#expectedOutData$STATIONS_ID[1] <- 666

results[i<-i+1] <- compare("comparison", actualOutData, expectedOutData)

# TEST: diffused irradiance table

	inData <- readSingleFile("tests/sample.in.txt")
	irradianceTable <- extractIrradianceData(inData)
	actualOutData <- pivotByYear(irradianceTable, "DIFFUSED")
	expectedOutData <- readSingleFile("tests/sample.out_diffused.txt")

	results[i<-i+1] <- compare("diffused irradiance", actualOutData, expectedOutData)

# TEST: global irradiance table

	inData <- readSingleFile("tests/sample.in.txt")
	actualOutData <- pivotByYear(extractIrradianceData(inData), "GLOBAL")
	expectedOutData <- readSingleFile("tests/sample.out_global.txt")

	results[i<-i+1] <- compare("global irradiance", actualOutData, expectedOutData)

# TEST: direct irradiance table

	inData <- readSingleFile("tests/sample.in.txt")
	actualOutData <- pivotByYear(extractIrradianceData(inData), "DIRECT")
	expectedOutData <- readSingleFile("tests/sample.out_direct.txt")

	results[i<-i+1] <- compare("direct irradiance", actualOutData, expectedOutData)

# TEST: other data extraction

	inData <- readSingleFile("tests/sample.in.txt")
	actualOutData <- extractOtherData(inData)
	expectedOutData <- readSingleFile("tests/sample.out_other_data.txt", colClasses = c("integer", "Date", "Date"))

	results[i<-i+1] <- compare("other data", actualOutData, expectedOutData)

# TEST: in leap year 29th of February is skipped

	inData <- readSingleFile("tests/sampleLeapYear.in.txt")
	irradiance <- extractIrradianceData(inData)

	# todo: improve leap year test
	actualOutData <- totalByMonth(irradiance)
	expectedOutData <- readSingleFile("tests/sample.out_leapYear.txt")

	results[i<-i+1] <- compare("leap year", actualOutData, expectedOutData)

# TEST: average irradiance for each hour in year

	inData <- readSingleFile("tests/sample.in.txt")
	irradiance <- extractIrradianceData(inData, skip29thFeb=FALSE)

	avgsByHourInYear <- averageByHourInYear(irradiance)

	actualOutData <- avgsByHourInYear
	expectedOutData <- readSingleFile("tests/sample.out_avgs_by_hour.txt")

	results[i<-i+1] <- compare("average irradiance for each hour in year", actualOutData, expectedOutData)

# TEST: average irradiance for each hour of the day in month

	inData <- readSingleFile("tests/sample.in.txt")
	irradiance <- extractIrradianceData(inData)

	avgsByHourOfDayAndMonth <- averageByHourOfDayAndMonth(irradiance)
	# todo: test
	#print(avgsByHourOfDayAndMonth)

	# actualOutData <- avgsByHourOfDayAndMonth
	# expectedOutData <- readSingleFile("tests/sample.out_avgs_by_hour.txt")

	# results[i<-i+1] <- compare("average irradiance for each hour in year", actualOutData, expectedOutData)

# TEST: total irradiance for each month

	inData <- readSingleFile("tests/sample.in.txt")
	irradiance <- extractIrradianceData(inData)

	sumByMonth <- totalByMonth(irradiance)
	# todo: test
	#print(sumByMonth)

	# actualOutData <- sumByMonth
	# expectedOutData <- readSingleFile("tests/sample.out_avgs_by_hour.txt")

	# results[i<-i+1] <- compare("average irradiance for each hour in year", actualOutData, expectedOutData)

# TEST: total irradiance for each month in each year

	inData <- readSingleFile("tests/sample.in.txt")
	irradiance <- extractIrradianceData(inData)

	sumByMonthInYears <- totalByMonthInYears(irradiance)

	actualOutData <- sumByMonthInYears
	expectedOutData <- readSingleFile("tests/sample.out_sums_by_month_in_years.txt")

	results[i<-i+1] <- compare("total irradiance for each month in each year", actualOutData, expectedOutData)

#------------------------------------------------------------------------------

print("")
if (length(results) == 0) {
	print("ALL TESTS PASSED")
} else {
	print("FAILED TESTS:")
print(results)
}

#------------------------------------------------------------------------------
