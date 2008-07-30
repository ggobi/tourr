#X t1 <- save_history(mtcars[, 1:5], nbases = 1000, interpolate = T, d = 1)
#X andrews_history(t1)
#X t2 <- save_history(mtcars[, 1:5], nbases = 1000, interpolate = T, d = 2)
#X andrews_history(t2)

andrews_history <- function(history, data = attr(history, "data")) {
  n <- dim(history)[3]

  x <- apply(history, 3, function(proj) data %*% proj)
  # x <- apply(x, 2, function(vec) {vec-mean(vec)})

  plot(c(1, n), range(x), type="n", xlab="Time", ylab="Data",
       main="Andrews curves from random projections")
  rect(-100, -5, n + 100, 5, col = "grey90")
  abline(h = seq(-3, 3, 0.5), col = "white")
  abline(v = seq(0, n, 50), col = "white")
      
  ys <- as.vector(t(cbind(x, NA)))
  xs <- rep(c(seq_len(n), NA), length = length(ys))
  lines(xs, ys)
}