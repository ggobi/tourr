#' Holes index.
holes <- function(mat) {
  n <- nrow(mat)
  d <- ncol(mat)

  num <- 1 - 1/n * sum(exp(-0.5 * rowSums(mat ^ 2)))
  den <- 1 - exp(-d / 2)
  
  num/den
}

#' Central mass index.
cm <- function(mat) 1 - holes(mat)

#' LDA projection pursuit index.
lda_pp <- function(mat, cl) {
  if (length(unique(cl)) < 2)
    return(NA)
  fit <- manova(mat ~ cl)                      

  1 - summary(fit,test="Wilks")$stats[[3]]
}


