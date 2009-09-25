# source("\\\\.PSF\\.Home\\ggobi\\tourr\\load.r")
# library(tourr)

FILE <- (function() {
  attr(body(sys.function()), "srcfile")
})()$filename
PATH <- dirname(FILE)

if (!exists("flea")) load(file.path(PATH, "data", "flea.rda"))
if (!exists("laser")) load(file.path(PATH, "data", "laser.rda"))
if (!exists("olive")) load(file.path(PATH, "data", "olive.rda"))
if (!exists("ozone")) load(file.path(PATH, "data", "ozone.rda"))
if (!exists("places")) load(file.path(PATH, "data", "places.rda"))
if (!exists("ratcns")) load(file.path(PATH, "data", "ratcns.rda"))
if (!exists("sleep")) load(file.path(PATH, "data", "sleep.rda"))
if (!exists("tao")) load(file.path(PATH, "data", "tao.rda"))

lapply(dir(file.path(PATH, "R"), full.name=TRUE), source)
