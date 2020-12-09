### Name: andrews
### Title: Compute Andrews' curves
### Aliases: andrews

### ** Examples

a <- andrews(1:2)
a(0)
a(-pi)
grid <- seq(-pi, pi, length = 50)
a(grid)

plot(grid, andrews(1:2)(grid), type = "l")
plot(grid, andrews(runif(5))(grid), type = "l")



