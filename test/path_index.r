### Name: path_index
### Title: Compute index values for a tour history.
### Aliases: path_index
### Keywords: hplot

### ** Examples

fl_holes <- save_history(flea[, 1:6], guided_tour(holes()), sphere = TRUE)
path_index(fl_holes, holes())
path_index(fl_holes, cmass())

plot(path_index(fl_holes, holes()), type = "l")
plot(path_index(fl_holes, cmass()), type = "l")

# Use interpolate to show all intermediate bases as well
## Not run: 
##D hi <- path_index(interpolate(fl_holes), holes())
##D hi
##D plot(hi)
## End(Not run)



