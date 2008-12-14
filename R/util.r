# Utility function for standardizing a vector of data
rescale <- function(df) {
  apply(df, 2, function(x) (x - min(x)) / diff(range(x)))
}

# Utility function for sphering a matrix of data
sphere <- function(df) {
  apply(predict(princomp(df)), 2, scale)
}
