source("genericplot.R")
source("plot2.R")
source("plot3.R")

plotvoltage <- function(dframe) {
  plot(dframe$DateTime,
       dframe$Voltage,
       type = "l",
       xlab = "datetime",
       ylab = "Voltage")
}

plotgrp <- function(dframe) {
  plot(dframe$DateTime,
       dframe$Global_reactive_power,
       type = "l",
       xlab = "datetime",
       ylab = "Global_reactive_power")
}

plot4 <- function(dframe) {
  par(mfcol = c(2, 2))
  plot2(dframe)
  plot3(dframe)
  plotvoltage(dframe)
  plotgrp(dframe)
}

genericplot(plot4)
