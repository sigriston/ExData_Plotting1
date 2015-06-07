##
## plot4.R - generates plot 4 for course project 1
## ----------------------------------------------------------
## "Exploratory Data Analysis" Course Project 1:
## https://class.coursera.org/exdata-015
## ----------------------------------------------------------
## Solution (c) Thiago Sigrist 2015.
##

library(downloader)
library(dplyr)
library(lubridate)

dldata <- function() {
  ##
  ## dldata() - downloads data files if necessary. If the data file is
  ##            already present, either in the current working directory or
  ##            in a subdirectory of it called "data" (preferred), the path
  ##            to the existing file is returned.
  ##
  ##            Otherwise, the file is downloaded to the current working
  ##            directory and unzipped to the "data" subdirectory, and the
  ##            path to the uncompressed file is returned.
  ##
  ## Return Value:
  ## -------------
  ## Path to the data file, ready to use no matter whether the file was
  ## already present or just downloaded.
  ##

  dlurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  destfile <- "household_power_consumption.zip"

  dpath1 <- "household_power_consumption.txt"
  dpath2 <- file.path("data", dpath1)

  if (file.exists(dpath2)) {
    return(dpath2)
  } else if (file.exists(dpath1)) {
    dir.create("data", showWarnings = FALSE)
    file.rename(dpath1, dpath2)
    return(dpath2)
  } else {
    download(dlurl, destfile)
    unzip(destfile, exdir = "data")
    return(dpath2)
  }
}

findlines <- function(f,
                      from = as.Date("2007-02-01"),
                      to = as.Date("2007-02-02")) {
  ##
  ## findlines() - find which lines in the file have dates between
  ##               the "from" and the "to" dates (inclusive).
  ##
  ## Arguments:
  ## ----------
  ## f    - path to file
  ## from - initial date (inclusive)
  ## to   - final date (inclusive)
  ##
  ## Return Value:
  ## -------------
  ## A R vector with values named "skip" and "nrows". These values are meant
  ## to be passed exactly to the "skip" and "nrows" arguments of read.table(),
  ## so that only the exact lines corresponding to the range between the
  ## "from" and "to" dates are read back.
  ##

  filecon <- file(f)
  open(filecon)

  readLines(filecon, n = 1)
  lineidx <- 1

  firstline <- 0
  lastline <- 0

  while (length(currline <- readLines(filecon, n = 1)) == 1) {
    lineidx <- lineidx + 1
    splitln <- strsplit(currline, ";", fixed = TRUE)
    linedate <- as.Date(splitln[[1]][1], "%d/%m/%Y")

    if (linedate == from && firstline == 0) {
      firstline <- lineidx
    }

    if (linedate == to) {
      lastline <- lineidx
    }

    if (linedate > to) {
      break
    }
  }

  close(filecon)

  c(skip = firstline - 1, nrows = lastline - firstline + 1)
}

readdata <- function() {
  ##
  ## readdata() - reads data from the data file into a data.frame.
  ##
  ## Return Value:
  ## -------------
  ## A R data.frame containing the data.
  ##

  datafile <- dldata()

  lineinfo <- findlines(datafile)

  cnames <- readLines(datafile, n = 1)
  cnames <- strsplit(cnames, ";", fixed = TRUE)[[1]]

  read.table(datafile,
             skip = lineinfo["skip"],
             nrows = lineinfo["nrows"],
             col.names = cnames,
             sep = ";",
             na.strings = "?",
             stringsAsFactors = FALSE)
}

processdata <- function(d) {
  ##
  ## processdata() - processes the data.frame read from the data file to
  ##                 make it tidier and easier to work.
  ##                 More specifically, replace the "Date" and "Time"
  ##                 character fields with a "datetime" field that is of
  ##                 type date+time (probably POSIXct + POSIXt or whatever
  ##                 else lubridate returns).
  ##
  ## Arguments:
  ## ----------
  ## d - the data.frame.
  ##
  ## Return Value:
  ## -------------
  ## R data.frame (dplyr tbl_df actually) with processed data.
  ##

  d %>%
    mutate(datetime = dmy(Date) + hms(Time)) %>%
    select(datetime, Global_active_power:Sub_metering_3)
}

tidydata <- function() {
  ##
  ## tidydata() - returns a data.frame with the data already tidied up for
  ##              this exercise.
  ##              This function is "smart" and will cache the results in a
  ##              "data/cached.rds" file.
  ##
  ## Return Value:
  ## -------------
  ## R data.frame (dplyr tbl_df actually) with the tidy data.
  ##

  cachedfile <- file.path("data", "cached.rds")

  if (file.exists(cachedfile)) {
    dframe <- readRDS(cachedfile)
  } else {
    dframe <- readdata()
    dframe <- processdata(dframe)
    saveRDS(dframe, cachedfile)
  }
  dframe
}

genericplot <- function(plotfn) {
  ##
  ## genericplot() - sets up the plotting environment (graphics device),
  ##                 loads up the data and then calls the plotfn function
  ##                 to do the real plotting work.
  ##                 This function was create to avoid repeating all this
  ##                 setup code inside the plot1, plot2, ... plotn
  ##                 functions.
  ##
  ## Arguments:
  ## ----------
  ## plotfn(dframe) - R function that receives a data.frame "dframe" and
  ##                  performs arbitrary plotting of data from dframe.
  ##

  plotfnname <- deparse(substitute(plotfn))
  plotfile <- paste0(plotfnname, ".png")

  png(plotfile, bg = "transparent")

  dframe <- tidydata()
  plotfn(dframe)

  invisible(dev.off())
}

plot2 <- function(dframe) {
  plot(dframe$datetime,
       dframe$Global_active_power,
       type = "l",
       xlab = "",
       ylab = "Global Active Power (kilowatts)")
}

plot3 <- function(dframe) {
  plot(dframe$datetime,
       dframe$Sub_metering_1,
       type = "l",
       xlab = "",
       ylab = "Energy sub metering")
  points(dframe$datetime, dframe$Sub_metering_2, type = "l", col = "red")
  points(dframe$datetime, dframe$Sub_metering_3, type = "l", col = "blue")

  legend("topright",
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         lty = 1,
         col = c("black", "red", "blue"))
}

plotvoltage <- function(dframe) {
  with(dframe,
       plot(datetime,
            Voltage,
            type = "l"))
}

plotgrp <- function(dframe) {
  with(dframe,
       plot(datetime,
            Global_reactive_power,
            type = "l"))
}

plot4 <- function(dframe) {
  par(mfcol = c(2, 2))
  plot2(dframe)
  plot3(dframe)
  plotvoltage(dframe)
  plotgrp(dframe)
}

genericplot(plot4)
