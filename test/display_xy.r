### Name: display_xy
### Title: Display tour path with a scatterplot
### Aliases: display_xy animate_xy

### ** Examples

animate_xy(flea[, 1:6])
animate(flea[, 1:6], tour_path = grand_tour(), display = display_xy())
animate(flea[, 1:6],
  tour_path = grand_tour(),
  display = display_xy(axes = "bottomleft")
)
animate(flea[, 1:6],
  tour_path = grand_tour(),
  display = display_xy(half_range = 0.5)
)
animate_xy(flea[, 1:6], tour_path = little_tour())
animate_xy(flea[, 1:3], tour_path = guided_tour(holes()), sphere = TRUE)
animate_xy(flea[, 1:6], center = FALSE)

# The default axes are centered, like a biplot, but there are other options
animate_xy(flea[, 1:6], axes = "bottomleft")
animate_xy(flea[, 1:6], axes = "off")
animate_xy(flea[, 1:6], dependence_tour(c(1, 2, 1, 2, 1, 2)),
  axes = "bottomleft"
)

animate_xy(flea[, -7], col = flea$species)

# You can also draw lines
edges <- matrix(c(1:5, 2:6), ncol = 2)
animate(
  flea[, 1:6], grand_tour(),
  display_xy(axes = "bottomleft", edges = edges)
)



