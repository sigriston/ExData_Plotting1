source("tidydata.R")

plot1 <- function() {
  png("plot1.png", bg = "transparent")
  dframe <- tidydata()
  hist(dframe$Global_active_power,
       col = "red",
       main = "Global Active Power",
       xlab = "Global Active Power (kilowatts)")
  invisible(dev.off())
}

plot1()
