library(ggplot2)

source("basis-generation.r")

one <- matrix(c(0, -1), ncol = 1)

samples <- function(alpha, method, n = 200) {
  data.frame(
    t(replicate(n, basis_nearby(one, alpha=alpha, method=method))),
    alpha = alpha,
    method = method
  )
}

all <- rbind(
  samples(1.0, "linear"),
  samples(0.8, "linear"),
  samples(0.6, "linear"),
  samples(0.4, "linear"),
  samples(0.2, "linear"),
  samples(1.0, "geodesic"),
  samples(0.8, "geodesic"),
  samples(0.6, "geodesic"),
  samples(0.4, "geodesic"),
  samples(0.2, "geodesic")
)

ggplot(all, aes(X1, X2)) + geom_point(colour = alpha("black", 1/10)) + facet_grid(method ~ alpha) + coord_equal() + geom_point(data= data.frame(X1 = 0, X2 = -1), colour="red")
