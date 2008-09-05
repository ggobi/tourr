# input <- basis_random(5, 2)
# frozen <- matrix(NA, nrow = 5, ncol = 2)
# frozen[1, 1] <- .1
# frozen[3, ] <- .5

freeze <- function(input, frozen) {
  fixed <- !is.na(frozen)
  
  input[fixed] <- NA
  input <- normalise(input)

  frozen_lengths <- colSums(frozen ^ 2, na.rm = TRUE)
  
  input <- sweep(input, 2, sqrt(1 - frozen_lengths), "*")
  input[fixed] <- frozen[fixed]
  
}