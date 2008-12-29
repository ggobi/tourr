# A frozen tour fixes some of the values of the orthonormal projection
# matrix and allows the others to vary freely according to any of the
# other tour methods.  This frozen tour is a frozen grand tour - frozen
# variations of the other tours are possible but are complex.  For
# example, with the guided tour, it is also necessary to ensure that the
# basis search method only searches the restricted subspace.
# 
# Some terminology:
#   * frozen variables: the variables that have fixed values
#   * warm variables: the remaining variables that vary freely
# 
# Unlike all the other tour methods, the frozen tour uses its own 
# geodesic method
# 
# @examples
# frozen <- matrix(NA, nrow = 4, ncol = 2)
# frozen[3, ] <- .5
# animate_xy(flea[, 1:4], frozen_tour(2, frozen))
# 
# # Doesn't work - a bug?
# frozen <- matrix(NA, nrow = 4, ncol = 2)
# frozen[1, 1] <- 0.5
# animate_xy(flea[, 1:4], frozen_tour(2, frozen))
# 
# # Doesn't work - a bug?
# frozen <- matrix(NA, nrow = 4, ncol = 2)
# frozen[, 1] <- 1/2
# animate_xy(flea[, 1:4], frozen_tour(2, frozen))
# 
# # Doesn't work - a bug?
# frozen[3, ] <- c(0, 1)
# animate_xy(flea[, 1:4], frozen_tour(2, frozen))
# 
# # Doesn't move, which is correct - no free variables
# frozen[4, ] <- .2
# animate_xy(flea[, 1:4], frozen_tour(2, frozen))
#
# # Doesn't work - a bug?
# frozen <- matrix(NA, nrow = 4, ncol = 2)
# frozen[, 1] <- 1/2
# animate_xy(flea[, 1:4], frozen_tour(2, frozen))
#
# # Two frozen variables in five 5.
# frozen <- matrix(NA, nrow = 5, ncol = 2)
# frozen[3, ] <- .5
# frozen[4, ] <- c(-.2, .2)
# animate_xy(flea[, 1:5], frozen_tour(2, frozen))
frozen_tour <- function(d = 2, frozen) { 
  
  frozen_geodesic <- function(previous, data) {
    if (is.null(previous)) return(basis_init(ncol(data), d))
    new_frozen <- function() {
      mat <- basis_random(ncol(data), d)      
      freeze(mat, frozen)
    }
    
    previous_frozen <- freeze(previous, frozen)
    
    dist <- 0
    while (dist < 1e-3) {
      frame <- new_frozen()
      dist <- proj_dist(previous_frozen, frame)
    }
    interpolator <- geodesic(previous_frozen, frame)
    
    interpolate <- function(pos) {
      thaw(step_fraction(interpolator, pos), frozen)
    }

    list(
      frame = frame,
      frame_prev = previous,
      interpolate = interpolate,
      dist = dist,
      tau = interpolator$tau,
      Ga = interpolator$Ga,
      Gz = interpolator$Gz
    )
  }
  
  structure(frozen_geodesic, class = "tour-path")
}


#' Freeze and thaw matrices
#'
#' @keywords internal
#' @examples
#' frozen <- matrix(NA, nrow = 4, ncol = 2)
#' frozen[3, ] <- .5
#'
#' input <- basis_random(4, 2)
#' freeze(input, frozen)
#' thaw(input, frozen)
#' freeze(basis_random(4, 2), frozen)
freeze <- function(input, frozen) {
  fixed <- !is.na(frozen)
  input[fixed] <- 0
  input
}

thaw <- function(input, frozen) {
  fixed <- !is.na(frozen)

  input <- normalise(input)
  frozen_lengths <- colSums(frozen ^ 2, na.rm = TRUE)
  
  input <- sweep(input, 2, sqrt(1 - frozen_lengths), "*")
  input[fixed] <- frozen[fixed]
  
  input
}