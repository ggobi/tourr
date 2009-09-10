library(plyr)

paths <- dir("~/ggobi/ggobi/data", pattern = "\\.csv$", full = TRUE)
names(paths) <- gsub("\\.csv", "", basename(paths))
all <- llply(paths, read.csv, row.names = 1) 

l_ply(names(all), function(name) {
  data <- all[[name]]
  names(data) <- tolower(names(data)) 
  assign(name, data)
  save(list = name, file = paste("data/", name, ".rda", sep = ""))
})