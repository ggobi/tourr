f.sphere.data <- function(x) {
  evc <- eigen(var(x))
  vc2 <- (evc$vectors)%*%diag(1/sqrt(evc$values))%*%t(evc$vectors)
  x <- x%*%vc2
}
