#X t1 <- save_history(flea[, 1:6], nbases = 1000, interpolate = T, d = 1)
#X andrews_history(t1)

andrews_history <- function(history, data = attr(history, "data"), center = TRUE) {
  n <- dim(history)[3]

  x <- apply(history, 3, function(proj) data %*% proj)
  if (center) x <- scale(x, center = TRUE, scale = FALSE)

  par(pty="m",mar=c(4,4,1,1))
  plot(c(1, n), range(x), type="n", xlab="Time", ylab="Data")
  rect(-100, -5, n + 100, 5, col = "grey90")
  abline(h = seq(-3, 3, 0.5), col = "white")
  abline(v = seq(0, n, 50), col = "white")
      
  ys <- as.vector(t(cbind(x, NA)))
  xs <- rep(c(seq_len(n), NA), length = length(ys))
  lines(xs, ys)
}
