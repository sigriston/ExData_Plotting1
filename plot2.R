source("genericplot.R")

plot2 <- function(dframe) {
  plot(dframe$DateTime,
       dframe$Global_active_power,
       type = "l",
       xlab = "",
       ylab = "Global Active Power (kilowatts)")
}

genericplot(plot2)
