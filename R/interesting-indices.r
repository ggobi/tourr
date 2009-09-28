#' Holes index.
#'
#' Calculates the holes index. See Cook and Swayne (2007)
#' Interactive and Dynamic Graphics for Data Analysis for equations.
#'
#' @param mat matrix being used
#' @keywords hplot
holes <- function(mat) {
  n <- nrow(mat)
  d <- ncol(mat)

  num <- 1 - 1/n * sum(exp(-0.5 * rowSums(mat ^ 2)))
  den <- 1 - exp(-d / 2)
  
  num/den
}



#' Central mass index.
#'
#' Calculates the central mass index.  See Cook and Swayne (2007)
#' Interactive and Dynamic Graphics for Data Analysis for equations.
#'
#' @param mat matrix being used
#' @keywords hplot
cm <- function(mat)
  1 - holes(mat)



#' LDA projection pursuit index.
#'
#' Calculate the LDA projection pursuit index.  See Cook and Swayne (2007)
#' Interactive and Dynamic Graphics for Data Analysis for equations.
#' 
#' @param cl class to be used.  Such as "color"
#' @keywords hplot
#'
lda_pp <- function(cl) {
  if (length(unique(cl)) < 2)
    stop("LDA index needs at least two classes")

  function(mat) {
    fit <- manova(mat ~ cl)                      

    1 - summary(fit,test="Wilks")$stats[[3]]    
  }
}

