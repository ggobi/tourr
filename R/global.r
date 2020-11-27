# to appease R CMD check
utils::globalVariables(
  c("tries", "index", "current", "cur_index", "t0", "record",
    "frozen", "id", "y", "past_x", "index_val", "info",
    "value", "obs"))

# Initialise global variables
if (getOption("tourr.verbose", default = FALSE))
  record <- NULL
