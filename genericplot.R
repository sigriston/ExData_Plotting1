source("tidydata.R")

genericplot <- function(plotfn) {
  plotfnname <- deparse(substitute(plotfn))
  plotfile <- paste0(plotfnname, ".png")

  png(plotfile, bg = "transparent")

  dframe <- tidydata()
  plotfn(dframe)

  invisible(dev.off())
}
