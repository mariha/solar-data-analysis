# solar-data-analysis
#------------------------------------------------------------------------------

# Data source: ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/solar/
# Source code: https://github.com/marycha/solar-data-analysis

#------------------------------------------------------------------------------
# Program structure
#------------------------------------------------------------------------------
# run.R - invokes main source code
# solar.R - main source code
# test.R - tests code
# tests/* - sample inputs and outputs used by tests
# database/* - input directory with zip files downloaded from ftp server
# output/* - output directory
# unzip/* - intermidiate input directory (once filled with unzipped files, we can reuse it and skip unzip phase)
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# To run the program:
#------------------------------------------------------------------------------

# Install R: https://www.r-project.org/
# Clone or download the code from GitHub repository to your working directory .
# Save zip files from datasource to ./database

# Open R console and set working directory (it should contain solar.R, run.R and test.R scripts)
setwd("path/to/your/working/directory")

# Install required packages (first run only)
install.packages("reshape")
install.packages("ggplot2")
install.packages("lubridate")

# Run tests
source("test.R")

# Run the code
source("run.R")

# Your results will be in ./output directory
#------------------------------------------------------------------------------
