solar-data-analysis
===================

General measurement method of an average sunlight power at a given location. The method is used to improve low-energy houses built by [Dworek Polski](www.dworekpolski.pl).

Data source: ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/solar

Source code: https://github.com/marycha/solar-data-analysis

Sample input and output data for Potsdam (Berlin) station is kept together with the source code.


Program structure
-----------------
* `run.R` - invokes main source code
* `solar.R` - main source code
* `test.R` - tests code
* `tests/*` - sample inputs and outputs used by tests
* `database/*` - input directory with zip files downloaded from ftp server
* `output/*` - output directory
* `unzip/*` - intermediate input directory (once filled with unzipped files, we can reuse it and skip unzip phase)


To run the program:
-------------------
1. Install R: https://www.r-project.org/
1. Clone or download the code from GitHub repository to your working directory `.`
1. Save zip files from datasource to `./database` (or use sample Potsdam data)
1. Open R console and set working directory (it should contain `solar.R`, `run.R` and `test.R` scripts)

        setwd("path/to/your/working/directory")
        
1. Install required packages (first run only)

        install.packages("reshape")
        install.packages("ggplot2")
        install.packages("lubridate")
        
1. Run tests

        source("test.R")

1. Run the code

        source("run.R")
    
1. Your results will be in `./output` directory
