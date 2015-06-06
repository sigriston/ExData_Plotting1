library(downloader)

download("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
         "household_power_consumption.zip")
unzip("household_power_consumption.zip", exdir = "data")
