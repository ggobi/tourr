### Name: render_gif
### Title: Render frames of animation to a gif file
### Aliases: render_gif

### ** Examples

# gifski needs to be installed to render a gif
if (requireNamespace("gifski", quietly = TRUE)) {
  gif_file <- file.path(tempdir(), "test.gif")
  render_gif(flea[, 1:4], grand_tour(), display_xy(), gif_file)
  utils::browseURL(gif_file)
  unlink(gif_file)
}



