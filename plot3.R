source("genericplot.R")

plot3 <- function(dframe) {
  plot(dframe$DateTime,
       dframe$Sub_metering_1,
       type = "l",
       xlab = "",
       ylab = "Energy sub metering")
  points(dframe$DateTime, dframe$Sub_metering_2, type = "l", col = "red")
  points(dframe$DateTime, dframe$Sub_metering_3, type = "l", col = "blue")

  legend("topright",
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         lty = 1,
         col = c("black", "red", "blue"))
}

genericplot(plot3)
