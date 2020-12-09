### Name: render
### Title: Render frames of animation to disk
### Aliases: render
### Keywords: hplot

### ** Examples

tmp_path <- tempdir()
render(flea[, 1:4], grand_tour(), display_xy(), "pdf",
  frames = 10,
  file.path(tmp_path, "test.pdf")
)
render(flea[, 1:4], grand_tour(), display_xy(), "png",
  frames = 10,
  file.path(tmp_path, "test-%03d.png")
)



