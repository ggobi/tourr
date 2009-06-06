# source("\\\\.PSF\\.Home\\ggobi\\tourr\\load.r")
# library(tourr)

FILE <- (function() {
  attr(body(sys.function()), "srcfile")
})()$filename
PATH <- dirname(FILE)

if (!exists("ozone")) load(file.path(PATH, "data", "ozone.rda"))
if (!exists("flea")) load(file.path(PATH, "data", "flea.rda"))

lapply(dir(file.path(PATH, "R"), full.name=T), source)
