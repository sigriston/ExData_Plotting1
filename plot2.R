source("tidydata.R")

plot2 <- function() {
  png("plot2.png", bg = "transparent")
  dframe <- tidydata()
  plot(dframe$DateTime,
       dframe$Global_active_power,
       type = "l",
       xlab = "",
       ylab = "Global Active Power (kilowatts)")
  invisible(dev.off())
}

plot2()
