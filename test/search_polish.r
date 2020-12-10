### Name: search_polish
### Title: Search very locally to find slightly better projections to
###   polish a broader search.
### Aliases: search_polish
### Keywords: optimize

### ** Examples

set.seed(2020)
t1 <- save_history(flea[, 1:6], guided_tour(holes()), max = 100)
attr(t1, "class") <- NULL
best_proj <- t1[, , dim(t1)[3]]
animate_xy(
  flea[, 1:6],
  guided_tour(holes(),
    search_f = search_polish,
    cur_index = 0
  ),
  start = best_proj
)



