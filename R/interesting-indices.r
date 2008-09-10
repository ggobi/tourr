holes <- function(mat) {
  n <- nrow(mat)
  d <- ncol(mat)

  num <- 1 - 1/n * sum(exp(-0.5 * rowSums(mat ^ 2)))
  den <- 1 - exp(-d / 2)
  
  num/den
}

cm <- function(mat) {
  n <- nrow(mat)
  d <- ncol(mat)

  num <- 1 / n * sum(exp(-0.5 * rowSums(mat ^ 2))) - exp(-d / 2)
  den <- 1 - exp(-d / 2)
  
  num/den
}

ldaPP <- function(mat, cl) {
  if (length(unique(cl)) < 2)
    return(NA)

  fit <- manova(mat~cl)

  1-summary(fit,test="Wilks")$stats[[3]]
}

#X r_tour(mtcars[, 1:5], guided_tour, index_f = cor1)
