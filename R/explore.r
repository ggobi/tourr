source("linear-algebra.r")

p <- 4

y <- replicate(10000, proj_dist(x, basis_random(4)))
qplot(y, geom="histogram", binwidth=0.01)


centre <- basis_random(4)

params <- data.frame(cool = seq(0.1, 1, by=0.1))
temp_dist <- function(cool, n=1000) {
  new_basis <- function() ortho(centre + cool * basis_random(4))
  data.frame(val = replicate(n, proj_dist(centre, new_basis())))
}

l(plyr)
all <- ddply(params, .(cool), splat(temp_dist))
qplot(val, data=all, geom="histogram", facets=cool~., binwidth=0.1)

