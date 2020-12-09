### Name: display_density2d
### Title: Display tour path with a density and scatterplot
### Aliases: display_density2d animate_density2d

### ** Examples

animate_density2d(flea[, 1:6])
animate(flea[, 1:6], tour_path = grand_tour(), display = display_density2d())
animate(flea[, 1:6],
  tour_path = grand_tour(),
  display = display_density2d(axes = "bottomleft")
)
animate(flea[, 1:6],
  tour_path = grand_tour(),
  display = display_density2d(half_range = 0.5)
)
animate_density2d(flea[, 1:6], tour_path = little_tour())

animate_density2d(flea[, 1:3], tour_path = guided_tour(holes()), sphere = TRUE)
animate_density2d(flea[, 1:6], center = FALSE)

# The default axes are centered, like a biplot, but there are other options
animate_density2d(flea[, 1:6], axes = "bottomleft")
animate_density2d(flea[, 1:6], axes = "off")
animate_density2d(flea[, 1:6], dependence_tour(c(1, 2, 1, 2, 1, 2)),
  axes = "bottomleft"
)

animate_density2d(flea[, -7], col = flea$species)

# You can also draw lines
edges <- matrix(c(1:5, 2:6), ncol = 2)
animate(
  flea[, 1:6], grand_tour(),
  display_density2d(axes = "bottomleft", edges = edges)
)



