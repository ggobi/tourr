# to appease R CMD check
utils::globalVariables(
  c("tries", "index", "current", "cur_index", "t0", "record",
    "frozen"))

# Global variable initialisation
record <- NULL
