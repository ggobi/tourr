### Name: guided_tour
### Title: A guided tour path.
### Aliases: guided_tour

### ** Examples

animate_xy(flea[, 1:6], guided_tour(holes()), sphere = TRUE)
animate_xy(flea[, 1:6], guided_tour(holes(), search_f = search_better_random), sphere = TRUE)
animate_dist(flea[, 1:6], guided_tour(holes(), 1), sphere = TRUE)
animate_xy(flea[, 1:6], guided_tour(lda_pp(flea[, 7])), sphere = TRUE, col = flea$species)

# save_history is particularly useful in conjunction with the
# guided tour as it allows us to look at the tour path in many different
# ways
f <- flea[, 1:3]
tries <- replicate(5, save_history(f, guided_tour(holes())), simplify = FALSE)



