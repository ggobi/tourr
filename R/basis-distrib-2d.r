# Testing distribution of target bases
source("linear-algebra.r")

A <- matrix(c(1, 0), nrow = 2)[, rep(1, 100)]
B <- replicate(100, basis_random(2, 1))

par(pty="s")

plot(t(B), xlab="x", ylab="y")
points(t(A), pch="x", col="red", cex=2)

C <- normalise(A + B)
points(0.9 * t(C), pch=16)
text(-0.1, 0.9, "1")

C <- normalise(A + 0.7 * B)
points(0.8 * t(C), pch=16)
text(-0.1, 0.8, "0.7")

C <- normalise(A + 0.5 * B)
points(0.7 * t(C), pch=16)
text(-0.1, 0.7, "0.5")

C <- normalise(A + 0.3 * B)
points(0.6 * t(C), pch=16)
text(-0.1, 0.6, "0.3")

C <- normalise(A + 0.1 * B)
points(0.5 * t(C), pch=16)
text(-0.1, 0.5, "0.1")