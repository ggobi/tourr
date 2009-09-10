hcl <- expand.grid(h = seq(0, 360, by = 10), l = 80, c = seq(0, 100, by = 10))
hcl <- transform(hcl, 
  angle = h * pi / 180,
  radius = c / diff(range(c))
)
hcl <- transform(hcl,
  x = radius * sin(angle),
  y = radius * cos(angle)
)
hcl$colour <- hcl(hcl$h, hcl$c, hcl$l, fixup=FALSE)

par(pty="s", mar=c(0,0,0,0), bg="grey85")
# with(hcl, plot(x, y, col=colour, pch=20, cex=3 , axes=FALSE, xlab="", ylab=""))


rgb_vals <- expand.grid(b = seq(0, 0.2, length=10), g = 0, r = seq(0.8, 1, length = 10))
rgb_vals$colour <- do.call(rgb, rgb_vals)

with(rgb_vals, plot(r, b, col=colour, pch=20, cex=8, axes=FALSE, xlab="", ylab=""))
with(rgb_vals, text(r, b, cex = 0.5, label = paste(format(r, TRUE, 2), format(g, TRUE, 2), format(b, TRUE, 2))))


# Blue: rgb(0, 0.91, 0.89)
# Red: rgb(0.98, 0.052, 0)
