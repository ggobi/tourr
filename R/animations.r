par(pch="s")
par(oma = c(0, 0, 0, 0))
par(mar = c(0, 0, 0, 0))
one <- matrix(c(0, -1), ncol = 1)

# Display path of 2->1d ------------------------------------------------------

target <- c(NA, NA)
start <- target
plot(NA, NA,xlim=c(-1.2,1.2), ylim=c(-1.2,1.2), xlab="", ylab="")
grand_tour(one, steps = Inf, velocity = 0.01,
  step_fun = function(step, proj) {
    Sys.sleep(0.01)
    rect(-1.2, -1.2, 1.2, 1.2, col=alpha("white", 0.20), border=NA)
    lines(rbind(t(target), -t(target)), col="red")
    lines(rbind(t(start), -t(start)), col="green")
    points(t(proj), cex=0.5, pch=20)
  },
  target_fun = function(proj) {
    start <<- target
    target <<- proj
    Sys.sleep(0.1)
  }
)


# 5d tour --------------------------------------------------------------------

cars <- t(apply(mtcars, 1, function(x) (x - min(x)) / diff(range(x))))
d5 <- cars[, 1:5]

plot(NA, NA,xlim=c(-2,2), ylim=c(-2,2), xlab="", ylab="")
grand_tour(basis_random(5, 2), steps = Inf, velocity = 0.1,
  step_fun = function(step, proj) {
    Sys.sleep(0.05)
    rect(-2, -2, 2, 2, col="#FFFFFFE6", border=NA)
    points(d5 %*% proj, pch=20)
})
