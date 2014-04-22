# f1 <- save_history(flea[, 1:6],grand_tour(d=1), max_bases = 10)
# x <- path1d_ggobi(f1)
# ggobi(x) 
path1d_ggobi <- function(basis, interp=TRUE, support=TRUE) {
  nc <- dim(basis)[1]

  if (interp) 
    basis_interp <- interpolate(basis)
  else
    basis_interp <- basis

  x <- matrix(f1interp[,1,],ncol=nc,byrow=T) 
  colnames(x) <- paste("a", 1:nc, sep="")

  if (support) {
    y <- NULL
    for (i in 1:1000) 
      y <- rbind(y, t(basis_random(nc, 1)))
    colnames(y) <- paste("a", 1:nc, sep="")
    x <- rbind(x, y)
    x <- cbind(x, c(rep(1, nrow(x)-1000), rep(0, 1000)))
    colnames(x)[nc+1] <- "PathorSupport"
  }
  x
}
