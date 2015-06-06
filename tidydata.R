source("readdata.R")

library(dplyr)
library(lubridate)

processdata <- function(d) {
  d %>%
    mutate(DateTime = dmy(Date) + hms(Time)) %>%
    select(DateTime, Global_active_power:Sub_metering_3)
}

tidydata <- function() {
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
