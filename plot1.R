source("genericplot.R")

plot1 <- function(dframe) {
  hist(dframe$Global_active_power,
       col = "red",
       main = "Global Active Power",
       xlab = "Global Active Power (kilowatts)")
}

genericplot(plot1)
