findlines <- function(f,
                      from = as.Date("2007-02-01"),
                      to = as.Date("2007-02-02")) {

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
  datafile <- file.path("data", "household_power_consumption.txt")

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
