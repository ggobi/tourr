#' Holes index.
#'
#' Finds the "interesting" index
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
#' Calculates the central mass index
#'
#' @param mat matrix being used
#' @keywords hplot
cm <- function(mat)
  1 - holes(mat)



#' LDA projection pursuit index.
#'
#' Calculate the LDA projection pursuit index
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

#' PDA projection pursuit index.
#'
#' Calculates the PDA projections pursuit index
#'
#' @param cl class to be used.  Such as "color"
#' @param lambda parameter used the "interesting" persuit.  It ranges from 0 to 1. O is lda_pp.  pda_pp is an updated form of lda_pp.
#' @keywords hplot
pda_pp <- function(cl, lambda) {
  if (length(unique(cl)) < 2) 
    stop("PDA index needs at least two classes")

  # require(classPP)
  function(mat){
    # PPindex.class("PDA",mat,cl,lambda=lambda)
    PPindex.PDA1(mat,cl,lambda)
  }
}
